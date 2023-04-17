# ---- load packages ----
suppressMessages(library(sf))
suppressMessages(library(nhdR))
suppressMessages(suppressWarnings(library(mapview)))
suppressMessages(library(nhdplusTools))
## turn on progress printing of dataRetrieval pulls:
## https://owi.usgs.gov/R/dataRetrieval.html#33
# library(httr)
# httr::set_config(httr::progress())
library(dataRetrieval)
suppressMessages(library(dplyr))
library(ggplot2)
library(stringr)
suppressMessages(library(prism))
if (dir.exists("prism")) {
  prism_set_dl_dir("prism")
}
suppressMessages(library(raster))
suppressMessages(library(FedData))
library(cdlTools)
suppressMessages(library(rdwplus))
suppressMessages(library(tsibble))
suppressMessages(library(fable))
suppressMessages(library(yardstick))
suppressMessages(library(rinvest)) # install_github("jsta/rinvest")
library(progress)
library(rnaturalearth)
suppressMessages(library(FME))
suppressMessages(library(LAGOSUS))
suppressMessages(library(tidyr))
suppressMessages(library(janitor))
library(cowplot)

# ---- helper functions ----

yahara_at_windsor <- "05427718"
year_default <- 2012

#' return the pour point of the nearest downstream lake
downstream_lake_get <- function(site_no) {
  # site_no <- "040871473"
  # site_no <- "05078470"
  # get downstream stream network
  nldi_feature <- list(
    featureSource = "nwissite",
    featureID = paste0("USGS-", site_no)
  )
  site <- nhdplusTools::get_nldi_feature(nldi_feature)
  stream_down <- nhdplusTools::navigate_nldi(nldi_feature,
    mode = "DM", distance_km = 20
  )


  if (length(stream_down) == 2 & !is.null(stream_down$DM_flowlines)) {
    # get lake polygon in buffer
    poly_buffer <- suppressWarnings(nhd_plus_query(
      lon = st_coordinates(site)[1], lat = st_coordinates(site)[2],
      dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = units::as_units(8, "km"),
      quiet = TRUE
    ))

    # get downstream lakes
    stream_down <- st_transform(
      stream_down$DM_flowlines,
      st_crs(poly_buffer$sp$NHDWaterbody)
    )
    waterbodies_down <- poly_buffer$sp$NHDWaterbody[
      unlist(lapply(
        st_intersects(poly_buffer$sp$NHDWaterbody, stream_down),
        function(x) length(x) > 0
      )),
    ]
    waterbodies_largest <- waterbodies_down[
      which.max(st_area(waterbodies_down)),
    ]

    pour_point <- suppressWarnings(st_intersection(
      stream_down,
      st_cast(waterbodies_largest,
        "MULTILINESTRING",
        group_or_split = FALSE
      )
    ))
    pour_point <- pour_point[
      which.min(st_distance(
        st_transform(site, st_crs(pour_point)), pour_point
      )),
    ]
  } else {
    pour_point <- NA
    waterbodies_down <- NA
  }

  list(
    pour_point = pour_point, waterbodies_down = waterbodies_down,
    site = site
  )
}

make_flow_and_phosphorus_path <- function(site_no, dates) {
  paste0(
    "nwis/",
    site_no, "_",
    strftime(dates$pull$start, "%Y"),
    "-",
    strftime(dates$forecast$end, "%Y"),
    ".rds"
  )
}

watershed_shp_path <- function(site_no) {
  paste0("watersheds/", site_no, ".shp")
}

dem_path <- function(site_no, burnfilled = FALSE) {
  if (burnfilled == TRUE) {
    paste0("ned/", site_no, "_NED_1_burnfilled.tif")
  } else {
    paste0("ned/", site_no, "_NED_1_projected.tif")
  }
}

precip_path <- function(site_no, year) {
  dir_path <- paste0("prism/", site_no)
  dir.create(dir_path, showWarnings = FALSE)

  paste0(dir_path, "/", year, ".tif")
}

#' @examples
#' \dontrun{
#' lulc_path(yahara_at_windsor, year)
#' }
lulc_path <- function(site_no, year, reclassify = FALSE) {
  dir_path <- paste0("cdl/", site_no)
  dir.create(dir_path, showWarnings = FALSE)

  if (reclassify == TRUE) {
    paste0(dir_path, "/", year, "_reclassify.tif")
  } else {
    paste0(dir_path, "/", year, ".tif")
  }
}

load_prism <- function(year) {
  prism_files <- pd_to_file(prism_archive_ls())

  prism_query_string <- paste0("PRISM_ppt_stable_4kmM3_", year, "_bil")

  prism_query_path <- prism_files[
    match(
      prism_query_string,
      tools::file_path_sans_ext(basename(prism_files))
    )
  ]

  raster::raster(prism_query_path)
}

# UTM 16N
ploading_crs <- st_crs(32616)
ploading_projstring <- as.character(sp::CRS("+init=epsg:32616"))

load_dem <- function(site_no) {
  # unlink(dem_path(site_no))
  # unlink("EXTRACTIONS/05431016/NED/05431016_NED_1.tif")

  watershed_shp <- st_read(watershed_shp_path(site_no), quiet = TRUE)
  watershed_buffer <- st_buffer(watershed_shp, 800)
  watershed_buffer$FID <- as.integer(watershed_buffer$FID)

  res <- suppressWarnings(
    FedData::get_ned(
      template = watershed_buffer, label = site_no,
      raw.dir = "ned/RAW",
      extraction.dir = "ned/EXTRACTIONS"
    )
  )

  # align to lulc extent and projection
  # the year is only important for finding lulc extent
  any_year <- as.numeric(substring(
    list.files(paste0("cdl/", site_no))[1],
    0, 4
  ))
  lulc <- raster(lulc_path(site_no, any_year, reclassify = TRUE))
  dem <- projectRaster(res, lulc)
  dem <- mask(dem, lulc)

  dem
}

crop_to_watershed_buffer <- function(r, watershed) {
  # watershed <- st_read(watershed_shp_path(site_no))
  # r <- raster(dem_path(site_no, burnfilled = TRUE))

  r_template <- rasterize(st_buffer(watershed, 30), r)
  r_mask <- mask(r, r_template)
  # mapview(r) + mapview(r_mask) + mapview(watershed)

  r_mask
}

burnfill_dem <- function(in_path, out_path) {
  # site_no <- "05408480"
  # in_path <- dem_path(site_no)
  # out_path <- dem_path(site_no, burnfilled = TRUE)
  initGRASS("/usr/lib/grass78", home = tempdir(), override = TRUE)

  invisible(capture.output(rdwplus::set_envir(in_path)))
  dem <- raster(in_path)
  # writeRaster(dem, "dem.tif", overwrite = TRUE)

  temp_streams_path <- paste0(tempdir(), "/temp_streams.shp")
  streams <- suppressWarnings(
    pull_streams(st_as_sfc(st_bbox(dem), crs = ploading_crs))
  )
  st_write(streams, temp_streams_path, append = FALSE, quiet = TRUE)

  invisible(rdwplus::raster_to_mapset(rasters = c(in_path), overwrite = TRUE))
  invisible(capture.output(
    rdwplus::vector_to_mapset(
      vectors = c(temp_streams_path),
      overwrite = TRUE
    )
  ))
  # execGRASS("g.list", parameters = list(type = c("raster", "vector")))

  streams_raster_path <- paste0(tempdir(), "/streams_rast.tif")
  invisible(capture.output(
    rdwplus::rasterise_stream("temp_streams", streams_raster_path,
      overwrite = TRUE
    )
  ))
  invisible(capture.output(
    rdwplus::reclassify_streams("streams_rast.tif", "streams_binary.tif",
      out_type = "binary", overwrite = TRUE
    )
  ))

  invisible(capture.output(
    rdwplus::burn_in(
      dem = basename(in_path), stream = "streams_binary.tif",
      out = "dem_burn.tif", burn = 10, overwrite = TRUE
    )
  ))
  # plot_GRASS("dem_burn.tif", col = topo.colors(5))

  invisible(capture.output(
    fill_sinks(
      dem = "dem_burn.tif", out = "dem_fill.tif",
      size = 1, overwrite = TRUE
    )
  ))
  # plot_GRASS("dem_fill.tif", col = topo.colors(5))


  # par(mfrow = c(3, 1))
  # plot(dem)
  # plot(raster(
  #   list.files(tempdir(), pattern = ".tif", full.names = TRUE, include.dirs = TRUE)[1]
  # ))
  # plot(raster(
  #
  #   ))
  # par(mfrow = c(1,1))

  rdwplus::retrieve_raster("dem_fill.tif", out_path, overwrite = TRUE)
  raster(out_path)
}

pull_streams <- function(bbox) {
  # bbox <- st_as_sfc(st_bbox(dem), crs = ploading_crs)
  streams <- nhdR::nhd_plus_query(
    poly = bbox,
    dsn = "NHDFlowLine", quiet = TRUE
  )
  streams <- st_crop(
    st_transform(streams$sp$NHDFlowLine, st_crs(bbox)),
    bbox
  )
  streams
}

reclassify_cdl <- function(lulc, cdl_reclassify_key) {
  values_not_in_key <- unique(values(lulc))[
    !(unique(values(lulc)) %in% cdl_reclassify_key$code_from)
  ]
  values_not_in_key <- values_not_in_key[!is.na(values_not_in_key)]
  values_not_in_key <- matrix(c(
    values_not_in_key,
    rep(0, length(values_not_in_key))
  ),
  ncol = 2, byrow = FALSE
  )

  rclmat <- matrix(c(cdl_reclassify_key[, 1], cdl_reclassify_key[, 2]),
    ncol = 2, byrow = FALSE
  )
  rclmat <- rbind(rclmat, values_not_in_key)
  rclmat <- rclmat[rclmat[, 1] != rclmat[, 2], ]

  log <- list()
  res_reclassify <- lulc
  for (i in seq_len(nrow(rclmat))) {
    # i <- 12
    res_reclassify <- reclassify(
      res_reclassify,
      matrix(rclmat[i, ], ncol = 2, byrow = TRUE)
    )
    log[[i]] <- c(
      "code_from" = rclmat[, 1][i],
      "in_result" = rclmat[, 1][i] %in% unique(values(res_reclassify))
    )
  }
  # any(dplyr::bind_rows(log)$in_result)

  # unique(values(res_reclassify))[order(unique(values(res_reclassify)))]
  # dplyr::distinct(dplyr::filter(cdl_reclassify_key,
  #             code_to %in% unique(values(res_reclassify))),
  #             code_to, description) %>% View()
  res_reclassify
}

#' NLCD colour map palettes
#'
#' @return A data frame with official class descriptions and hexencoded rgb(a) colour values
#' @importFrom raster values
#' @importFrom dplyr filter
#' @export
#' @references \url{https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend}
#' @examples
#' \dontrun{
#' # Extract data for the Village Ecodynamics Project 'VEPIIN' study area:
#' # http://village.anth.wsu.edu
#' vepPolygon <- polygon_from_extent(raster::extent(672800, 740000, 4102000, 4170000),
#'   proj4string = "+proj=utm +datum=NAD83 +zone=12"
#' )
#'
#' NLCD <- get_nlcd(template = vepPolygon, label = "VEPIIN")
#' NLCD <- as.matrix(table(raster::values(NLCD)))
#' cols <- dplyr::filter(pal_nlcd(), code %in% row.names(NLCD))
#'
#' par(xpd = TRUE, mar = c(10, 3, 2, 1))
#' barplot(NLCD, beside = FALSE, col = cols$color)
#' legend("bottom",
#'   legend = cols$description, fill = cols$color,
#'   ncol = 2, inset = c(0, -0.6)
#' )
#' }
pal_nlcd <- function() {
  data.frame(
    class = c(
      "water", "water",
      "developed", "developed", "developed", "developed",
      "barren",
      "forest", "forest", "forest",
      "shrubland", "shrubland",
      "herbaceous", "herbaceous", "herbaceous", "herbaceous",
      "planted", "planted",
      "wetlands", "wetlands"
    ),
    code = as.character(c(
      11, 12,
      21, 22, 23, 24,
      31,
      41, 42, 43,
      51, 52,
      71, 72, 73, 74,
      81, 82,
      90, 95
    )),
    description = c(
      "Open Water", "Perennial Ice/Snow",
      "Developed, Open Space", "Developed, Low Intensity",
      "Developed, Medium Intensity", "Developed, High Intensity",
      "Barren Land (Rock/Sand/Clay)",
      "Deciduous Forest", "Evergreen Forest", "Mixed Forest",
      "Dwarf Scrub", "Scrub/Shrub",
      "Grassland/Herbaceous", "Sedge/Herbaceuous", "Lichens", "Moss",
      "Pasture/Hay", "Cultivated Crops",
      "Woody Wetlands", "Emergent Herbaceous Wetlands"
    ),
    color = c(
      "#476BA0", "#D1DDF9",
      "#DDC9C9", "#D89382", "#ED0000", "#AA0000",
      "#B2ADA3",
      "#68AA63", "#1C6330", "#B5C98E",
      "#A58C30", "#CCBA7C",
      "#E2E2C1", "#C9C977", "#99C147", "#77AD93",
      "#DBD83D", "#AA7028",
      "#BAD8EA", "#70A3BA"
    ),
    stringsAsFactors = FALSE
  )
}

#' check if file args exist
ndr_file_args_exist <- function(args) {
  # args <- args_default

  file_args_index <- grep("path", names(args))
  args_exist <- unlist(lapply(
    file_args_index, function(i) {
      file.exists(as.character(args[i]))
    }
  ))

  if (any(!args_exist)) {
    missing_args <- file_args_index[!args_exist]
    missing_args <- names(args)[missing_args]
    stop(paste0(
      "The following ndr file arguments do not exist: ",
      paste0(missing_args, collapse = ", ")
    ))
  }
  invisible(NULL)
}

adjust_biophys <- function(p_load_calibration_fac) {
  biophys <- read.csv("biophysical_table_path.csv",
    stringsAsFactors = FALSE
  )
  biophys$load_p <- biophys$load_p * p_load_calibration_fac
  write.csv(biophys, "biophysical_table_path_temp.csv", row.names = FALSE)

  biophys
}

# zero_pad(10, 1)
# zero_pad(1, 1)
zero_pad <- function(x, digits, target = 2) {
  if (nchar(stringr::str_extract(x, "\\d+")) < target) {
    paste0(
      paste0(
        rep(0, digits),
        collapse = ""
      ), x,
      collapse = ""
    )
  } else {
    as.character(x)
  }
}

#' Detect if a USGS gage is upstream of a lake
#'
#' @param site_no character to preserve leading zeros
#' @param distance_threshold numeric in units of km
#'
#' @importFrom nhdplusTools get_nldi_feature navigate_nldi
#' @importFrom nhdR nhd_plus_query
#' @importFrom sf st_coordinates st_transform st_crs st_intersection st_distance st_area st_cast
#' @importFrom units set_units as_units
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' site_no <- "040871473"
#' is_lake_gage(site_no)$is_lake_gage # FALSE
#' site_no <- "05427718"
#' is_lake_gage(site_no)$is_lake_gage # TRUE
#' site_no <- "03208950"
#' site_no <- "7091500"
#' site_no <- "05558300"
#' site_no <- "05488110"
#' site_no <- "02492343"
#' site_no <- "05126210"
#' res <- is_lake_gage(site_no)
#' mapview::mapview(res$waterbodies_down)
#' }
is_lake_gage <- function(site_no, distance_threshold = 20) {
  # distance_threshold <- 20
  nldi_feature <- list(
    featureSource = "nwissite",
    featureID = paste0("USGS-", site_no)
  )
  site <- suppressMessages(
    nhdplusTools::get_nldi_feature(nldi_feature)
  )
  stream_down <- suppressMessages(tryCatch(
    nhdplusTools::navigate_nldi(nldi_feature,
      mode = "DM", distance_km = distance_threshold
    ),
    error = function(e) NA
  ))

  if (
    length(stream_down) == 2 &
      suppressWarnings(
        tryCatch(
          !is.null(stream_down$DM_flowlines),
          error = function(e) FALSE
        )
      )
  ) {
    # get lake polygons in buffer
    poly_buffer <- suppressMessages(suppressWarnings(nhd_plus_query(
      lon = st_coordinates(site)[1], lat = st_coordinates(site)[2],
      dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = units::as_units(8, "km"),
      quiet = TRUE
    )))
    if ("GNIS_Name" %in% names(poly_buffer$sp$NHDWaterbody)) {
      poly_buffer$sp$NHDWaterbody <- dplyr::rename(poly_buffer$sp$NHDWaterbody,
        GNIS_NAME = GNIS_Name
      )
    }
    if ("FCode" %in% names(poly_buffer$sp$NHDWaterbody)) {
      poly_buffer$sp$NHDWaterbody <- dplyr::rename(poly_buffer$sp$NHDWaterbody,
        FCODE = FCode
      )
    }

    # only proceed if there are *any* lakes within the distance_threshold
    real_lakes <- dplyr::filter(
      poly_buffer$sp$NHDWaterbody,
      !(.data$GNIS_NAME %in% c(
        "Lake Michigan", "Lake Superior",
        "Lake Ontario", "Lake Erie", "Lake Saint Lawrence"
      )) |
        is.na(.data$GNIS_NAME)
    )

    if ("ComID" %in% names(real_lakes)) {
      real_lakes$COMID <- real_lakes$ComID
    }
    real_lakes <- real_lakes %>%
      dplyr::filter(!(COMID %in% c(
        120053703, # lake st clair
        15176789 # atchafalaya bay
      )))
    real_lakes <- dplyr::filter(
      real_lakes,
      !(.data$FTYPE %in% c("SwampMarsh")),
      !(.data$FCODE %in% c(39001))
    ) # intermittent lakes
    real_lakes <- real_lakes[st_area(real_lakes) > units::as_units(4, "ha"), ]

    if (
      any(
        units::set_units(st_distance(
          st_transform(site, nhdR:::albers_conic()),
          st_transform(real_lakes, nhdR:::albers_conic())
        ), "km") <
          units::as_units(distance_threshold, "km")
      )
    ) {
      # get downstream lakes
      stream_down <- st_transform(
        stream_down$DM_flowlines,
        st_crs(poly_buffer$sp$NHDWaterbody)
      )
      waterbodies_down <- suppressMessages(real_lakes[
        unlist(lapply(
          st_intersects(real_lakes, stream_down),
          function(x) length(x) > 0
        )),
      ])
      waterbodies_largest <- waterbodies_down[
        which.max(st_area(waterbodies_down)),
      ]
      # pour point is based on largest waterbody so let's return it here
      waterbodies_down <- waterbodies_largest

      pour_point <- suppressWarnings(suppressMessages(
        st_intersection(stream_down, st_cast(waterbodies_largest,
          "MULTILINESTRING",
          group_or_split = FALSE
        ))
      ))
      pour_point <- pour_point[
        which.min(st_distance(
          st_transform(site, st_crs(pour_point)), pour_point
        )),
      ]
    } else {
      pour_point <- NA
      waterbodies_down <- NA
    }
  } else {
    pour_point <- NA
    waterbodies_down <- NA
  }

  is_lake_gage <- ifelse(
    !is.null(nrow(pour_point)),
    nrow(waterbodies_down) > 0,
    FALSE
  )

  list(
    pour_point = pour_point, waterbodies_down = waterbodies_down,
    site = site, is_lake_gage = is_lake_gage
  )
}

# from jsta::pdf_table
pdf_table <- function(x, out_name = "test.pdf") {
  if (is.data.frame(x)) {
    pdf(file = out_name)
    # Only fits 25 rows of data. Warn if greater?
    gridExtra::grid.table(x, rows = rep("", nrow(x)))
    dev.off()
    system(paste0("pdfcrop ", out_name, " ", out_name))
  }
  if (is.character(x)) {
    zz <- file("test.md", "w")
    sink(zz)
    cat("\\pagenumbering{gobble}")
    print(x)
    sink()
    close(zz)
    system(paste0("pandoc -s test.md -o ", out_name))
    system(paste0("pdfcrop ", out_name, " ", out_name))
    unlink("test.md")
  }
}

#' Merge custom parameter settings with defaults
#' @details Run from the top level folder for now
# params_custom <- data.frame(
#   median = c("background" = 2),
#   stringsAsFactors = FALSE)
ndr_params_default <- function(params_custom = NA) {
  if (file.exists("calibration/ndr_params_default.csv")) {
    params <- read.csv("calibration/ndr_params_default.csv",
      stringsAsFactors = FALSE
    )
  } else {
    params <- read.csv("../calibration/ndr_params_default.csv",
      stringsAsFactors = FALSE
    )
  }
  params_unpack <- tidyr::pivot_longer(params, !param)

  if (!inherits(params_custom, "data.frame")) {
    return(params)
  }
  params_custom$param <- row.names(params_custom)
  params_custom_unpack <- tidyr::pivot_longer(params_custom, !param)

  res <- dplyr::anti_join(params_unpack, params_custom_unpack,
    by = c("param", "name")
  )
  res <- rbind(res, params_custom_unpack)

  res_repack <- tidyr::pivot_wider(res, names_from = c(name))
  res_repack <- dplyr::select(res_repack, names(params))

  # dplyr::filter(res_repack, param == "background")
  # dplyr::filter(params, param == "background")

  # warn if median settings are outside the upper/lower range
  res_repack <- dplyr::mutate(res_repack,
    in_range = ((median <= upper) & (median >= lower))
  )
  if (any(!res_repack$in_range)) {
    warning(
      paste0(
        "Custom param '",
        res_repack$param[!res_repack$in_range],
        "' outside defined upper/lower range"
      )
    )
  }
  res_repack <- dplyr::select(res_repack, -in_range)

  res_repack
}



# site_no <- yahara_at_windsor
# year <- 2012
ndr_args_default <- function(site_no = yahara_at_windsor, year = year_default,
                             k_param = 2, threshold_flow_accumulation = 1000,
                             collected = FALSE) {
  if (!collected) {
    res <- list(
      "workspace_dir" = "workspace",
      "dem_path" = paste0("data/", dem_path(site_no, burnfilled = TRUE)),
      "lulc_path" = paste0(
        "data/",
        lulc_path(site_no, year_default, reclassify = TRUE)
      ),
      "runoff_proxy_path" = paste0("data/", precip_path(site_no, year)),
      "watersheds_path" = paste0("data/", watershed_shp_path(site_no)),
      "biophysical_table_path" = "data/biophys.csv",
      "calc_p" = TRUE,
      "calc_n" = FALSE,
      "threshold_flow_accumulation" = threshold_flow_accumulation,
      "k_param" = k_param,
      "subsurface_eff_p" = 0, # not used for p model
      "subsurface_critical_length_p" = 0, # not used for p model
      "subsurface_eff_n" = 0, # not used for p model
      "subsurface_critical_length_n" = 0 # not used for p model
    )
  } else {
    res <- list(
      "workspace_dir" = "workspace",
      "dem_path" = "workspace/dem_path.tif",
      "lulc_path" = "workspace/lulc_path.tif",
      "runoff_proxy_path" = "workspace/runoff_proxy_path.tif",
      "watersheds_path" = "workspace/watersheds_path.shp",
      "biophysical_table_path" = "workspace/biophys.csv",
      "calc_p" = TRUE,
      "calc_n" = FALSE,
      "threshold_flow_accumulation" = threshold_flow_accumulation,
      "k_param" = k_param,
      "subsurface_eff_p" = 0, # not used for p model
      "subsurface_critical_length_p" = 0, # not used for p model
      "subsurface_eff_n" = 0, # not used for p model
      "subsurface_critical_length_n" = 0 # not used for p model
    )
  }

  res
}

#' Run the InVEST NDR model
#'
#' @param p numeric vector of parameters of variable length. If set to less
#'  than 13, p_position must be specified to indicate correspondance with
#'  ndr_params_default() rows. Shorter p lengths "fix" the parameters not
#'  specified by p_position.
#' @param site_id character USGS site id
#' @param year numeric year
#' @param p_position integer vector matching the elements of p to the
#'  rows of ndr_params_default(). Required if length(p) < 13.
#' @param log_path file path to log location
#' @param load_fac_weight logical. weight loadings by load_fac (used for calibration)?
## @examples
# source("scripts/99_utils.R")
# run_ndr(ndr_params_default()$median, yahara_at_windsor, year_default)
run_ndr <- function(p, site_no, year, p_position = 1:14,
  log_path = "log.csv", load_fac_weight=TRUE) {
  if ( # error if too few entries in p and default p_position
    (length(p) != length(ndr_params_default()$median)) &
      (identical(p_position, 1:14))
  ) {
    stop("Too few (p)arameters passed to run_ndr")
  }

  params <- as.numeric(p)

  # substitute ndr_params_default with params based on p_position
  # if (length(params) < nrow(ndr_params_default())) {
  params_default <- ndr_params_default()$median
  params_default[p_position] <- params
  params <- params_default
  # }

  # write relevant params to biophys csv
  ## read "default" biophys from the data folder
  ## overwrite the load_p column with relevant params
  if (file.exists("data/biophys.csv")) {
    biophys_path <- "data/biophys.csv"
  } else {
    biophys_path <- "../data/biophys.csv"
  }
  # print(dir())
  biophys_default <- read.csv(biophys_path, stringsAsFactors = FALSE)
  params_load <- params[1:11] # extract load params
  # print(params_load)
  # print(params)
  biophys_default$load_p <- as.numeric(params_load)
  if(load_fac_weight){
     biophys_default$load_p <- biophys_default$load_p * as.numeric(params[14])
  }

  # print(params_load[1] * params[14])
  write.csv(biophys_default, biophys_path, row.names = FALSE)

  # push relevant params to their corresponding args field
  args <- ndr_args_default(site_no, year, collected = FALSE)
  args$k_param <- params[12]
  args$threshold_flow_accumulation <- params[13]

  # setup "workspace" folder
  unlink("workspace", recursive = TRUE)
  dir.create("workspace", showWarnings = FALSE)

  # prep for ndr
  collect_run_ndr(args, "workspace", symlink = TRUE)
  args <- read.csv("workspace/args.csv", stringsAsFactors = FALSE)
  setwd("workspace")
  invisible(
    file.copy(
      paste0(
        dirname(args$biophysical_table_path),
        "/biophysical_table_path.csv"
      ),
      args$biophysical_table_path,
      overwrite = TRUE
    )
  )

  # execute ndr
  ndr(as.list(args), overwrite = TRUE)

  # return rmse
  p_export_invest <- rinvest::ndr_p_export_total(".")
  print(paste0("invest total P export: ", round(p_export_invest, 2)))

  nwis_path <- "data/nwis/nwis.csv"
  if(!file.exists(nwis_path)){
    nwis_path <- "../../data/nwis/nwis.csv"
    if(!file.exists(nwis_path)){
      nwis_path <- "../data/nwis/nwis.csv"
    }
  }
  p_export_sparrow <- read.csv(nwis_path,
    colClasses = c("flow_station_id" = "character")
  ) %>%
    dplyr::filter(flow_station_id == site_no) %>%
    pull(LOAD_A_00665) # kg/yr
  # browser()
  # print(paste0("sparrow total P export: ", round(p_export_sparrow, 2)))

  setwd("../")

  rmse <- sqrt(mean((p_export_sparrow - p_export_invest)^2))
  rmse <- round(rmse, 2)
  # print(rmse)
  pe <- (abs(p_export_invest - p_export_sparrow) / p_export_sparrow) * 100

  time_and_fit <- data.frame(
    datetime = Sys.time(), fit = rmse,
    p_export_sparrow = round(p_export_sparrow, 2),
    p_export_invest = round(p_export_invest, 2),
    stringsAsFactors = FALSE
  )
  log_param <- data.frame(t(biophys_default$load_p),
    stringsAsFactors = FALSE
  )
  log_param <- setNames(log_param, biophys_default$description)
  log_arg <- data.frame(
    args[, c("k_param", "threshold_flow_accumulation")],
    stringsAsFactors = FALSE
  )
  log_load_fac <- data.frame("load_fac" = params[14], stringsAsFactors = FALSE)
  log <- cbind(log_param, log_arg, log_load_fac)
  log[1, ] <- round(log[1, ], 2)
  log <- cbind(time_and_fit, log)

  suppressWarnings(
    write.table(log, log_path,
      sep = ",", col.names = !file.exists(log_path), append = TRUE,
      row.names = FALSE
    )
  )

  if (rmse < 10 | pe < 5) {
    stop("Model converged. RMSE < 10 or PE < 5")
  } else {
    rmse
  }
}

calibration_viz <- function(log_path, save_fig = FALSE) {
  # log_path <- "calibration/log_2012_03208950.csv"

  site_no <- stringr::str_extract(log_path, "\\d{7,10}")

  log <- read.csv(log_path, stringsAsFactors = FALSE) %>%
    dplyr::mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))

  if ("k" %in% names(log)) {
    log$k_param <- log$k
  }

  # log_loads <- dplyr::select(log, datetime, Background:Wetlands) %>%
  # tidyr::pivot_longer(!datetime)

  res <- cowplot::plot_grid(
    # ggplot(data = log_loads) +
    # geom_line(aes(x = datetime, y = value, color = name)),
    ggplot(data = log) +
      geom_line(aes(x = datetime, y = load_fac)) +
      ggtitle(site_no),
    ggplot(data = log) +
      geom_line(aes(x = datetime, y = k_param)),
    ggplot(data = log) +
      geom_line(aes(x = datetime, y = threshold_flow_accumulation)),
    ggplot(data = log) +
      geom_line(aes(x = datetime, y = p_export_invest)) +
      geom_line(aes(x = datetime, y = p_export_sparrow), color = "red"),
    ncol = 1, rel_heights = c(1, rep(0.83, 3))
  )

  if (save_fig) {
    fig_path <- paste0(site_no, ".png")
    ggsave(fig_path, res)
  }

  print(res)
}

load_site_gis <- function(site_no, gpkg_path = "gis.gpkg") {
  # site_no <- "07360200"

  # gage point
  dt <- st_read(gpkg_path, "dt", quiet = TRUE)
  dt <- dt[dt$site_no == site_no, ]

  # lake polygon
  dt_waterbodies <- st_read(gpkg_path, "dt_waterbodies", quiet = TRUE)
  dt_waterbodies <- dt_waterbodies[dt_waterbodies$site_no == site_no, ]

  # watershed polygon
  dt_watersheds <- st_read(gpkg_path, "dt_watersheds", quiet = TRUE)
  dt_watersheds <- dt_watersheds[dt_watersheds$site_no == site_no, ]

  list(
    dt = dt,
    dt_waterbodies = dt_waterbodies,
    dt_watersheds = dt_watersheds
  )
}

# site_no    <- candidate_sites
get_watershed_shp <- function(site_no) {
  # site_no <- candidate_sites[[4]]
  # site_no <- "03208950"

  # ---- get watershed outline ----
  # invest expects a shapefile path as input
  shp_path <- watershed_shp_path(site_no)
  # unlink("data/watersheds/05427718.shp")
  if (!file.exists(shp_path) | !interactive()) {
    site_metadata <- list(
      featureSource = "nwissite",
      featureID = paste0("USGS-", as.character(site_no))
    )
    gage_pnt <- get_nldi_feature(site_metadata)
    basin <- get_nldi_basin(nldi_feature = site_metadata)
    basin <- st_transform(basin, ploading_crs)
    streams <- suppressMessages(suppressWarnings(
      nhdR::nhd_plus_query(st_coordinates(gage_pnt)[1],
        st_coordinates(gage_pnt)[2],
        dsn = "NHDFlowLine",
        buffer = units::as_units(1, "km"),
        quiet = TRUE
      )
    ))

    ## TODO: valid site if basin area is over a certain minimum area

    st_write(basin, shp_path, append = FALSE, quiet = TRUE)
  }

  st_read(shp_path, quiet = TRUE)
}
