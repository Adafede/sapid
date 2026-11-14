library(tinytest)

message("Preparation and napping path checks")
ns <- asNamespace("sapid")
prepare_chasselas <- get("prepare_chasselas", envir = ns)
prepare_profiles <- get("prepare_profiles", envir = ns)
prepare_napping <- get("prepare_napping", envir = ns)
plot_napping <- get("plot_napping", envir = ns)

orig_dir <- getwd()
fixture_candidates <- c(
  file.path(orig_dir, "fixtures", "session_fixture.xlsx"),
  file.path(orig_dir, "inst", "tinytest", "fixtures", "session_fixture.xlsx")
)
fixture <- fixture_candidates[file.exists(fixture_candidates)][1]

# 1. Error paths when required columns (e.g. 'CJ' or >=3 napping_words cols) are missing
withr::with_tempdir({
  input_dir <- getwd()
  xlsx_dir <- file.path(input_dir, "20210412_cluster6", "03_files")
  dir.create(xlsx_dir, recursive = TRUE)

  expect_true(file.exists(fixture))
  expect_true(file.copy(
    fixture,
    file.path(xlsx_dir, "session.xlsx"),
    overwrite = TRUE
  ))

  out_chasselas <- file.path(input_dir, "chasselas.tsv")
  expect_error(
    prepare_chasselas(
      input_dir = input_dir,
      sessions = 1L,
      output = out_chasselas
    ),
    pattern = "Column 'CJ' not found in session data\\."
  )

  out_profiles <- file.path(input_dir, "profiles.tsv")
  data(dictionary_generic, package = "sapid")
  data(dictionary_specific, package = "sapid")
  expect_error(
    prepare_profiles(
      input_dir = input_dir,
      sessions = 1L,
      output = out_profiles,
      dictionary_generic_path = dictionary_generic,
      dictionary_specific_path = dictionary_specific
    ),
    pattern = "Column 'CJ' not found in session data\\."
  )

  out_coordinates <- file.path(input_dir, "napping_coordinates.tsv")
  out_descriptors <- file.path(input_dir, "napping_descriptors.tsv")
  data(dictionary_napping, package = "sapid")
  expect_error(
    prepare_napping(
      input_dir = input_dir,
      sessions = 1L,
      output_coordinates = out_coordinates,
      output_descriptors = out_descriptors,
      dictionary_generic_path = dictionary_generic,
      dictionary_napping_path = dictionary_napping,
      dictionary_specific_path = dictionary_specific
    ),
    pattern = "There are only 2 columns in df; at least 3 are required\\."
  )
  expect_true(file.exists(out_coordinates))
})

# 2. Full happy paths for prepare_chasselas, prepare_profiles, and prepare_napping
withr::with_tempdir({
  input_dir <- getwd()
  xlsx_dir <- file.path(input_dir, "20210412_cluster6", "03_files")
  dir.create(xlsx_dir, recursive = TRUE)

  build_xlsx <- function(sheets_xml, dest) {
    td <- tempfile("xlsx_prep_")
    dir.create(file.path(td, "xl", "worksheets"), recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    for (i in seq_along(sheets_xml)) {
      writeLines(
        enc2utf8(sheets_xml[[i]]),
        file.path(td, "xl", "worksheets", paste0("sheet", i, ".xml")),
        useBytes = TRUE
      )
    }
    old_wd <- setwd(td)
    on.exit(setwd(old_wd), add = TRUE)
    utils::zip(dest, files = "xl", flags = "-rq")
  }

  # Sheet 1 (chasselas): ProductName, CJ, Date + all 9 French taste attributes
  sheet1 <- paste0(
    "<worksheet><sheetData>",
    "<row r=\"1\">",
    "<c r=\"A1\" t=\"inlineStr\"><is><t>ProductName</t></is></c>",
    "<c r=\"B1\" t=\"inlineStr\"><is><t>CJ</t></is></c>",
    "<c r=\"C1\" t=\"inlineStr\"><is><t>Date</t></is></c>",
    "<c r=\"D1\" t=\"inlineStr\"><is><t>acide_1</t></is></c>",
    "<c r=\"E1\" t=\"inlineStr\"><is><t>amer_1</t></is></c>",
    "<c r=\"F1\" t=\"inlineStr\"><is><t>sucr\u00e9</t></is></c>",
    "<c r=\"G1\" t=\"inlineStr\"><is><t>sal\u00e9</t></is></c>",
    "<c r=\"H1\" t=\"inlineStr\"><is><t>gras</t></is></c>",
    "<c r=\"I1\" t=\"inlineStr\"><is><t>\u00e9quilibre</t></is></c>",
    "<c r=\"J1\" t=\"inlineStr\"><is><t>fraicheur</t></is></c>",
    "<c r=\"K1\" t=\"inlineStr\"><is><t>longueur</t></is></c>",
    "<c r=\"L1\" t=\"inlineStr\"><is><t>salivant</t></is></c>",
    "</row>",
    "<row r=\"2\">",
    "<c r=\"A2\" t=\"inlineStr\"><is><t>CHASAVANT</t></is></c>",
    "<c r=\"B2\" t=\"inlineStr\"><is><t>J_B</t></is></c>",
    "<c r=\"C2\" t=\"inlineStr\"><is><t>d20210412</t></is></c>",
    "<c r=\"D2\"><v>2.5</v></c>",
    "<c r=\"E2\"><v>1.5</v></c>",
    "<c r=\"F2\"><v>5.0</v></c>",
    "<c r=\"G2\"><v>1.0</v></c>",
    "<c r=\"H2\"><v>3.0</v></c>",
    "<c r=\"I2\"><v>7.0</v></c>",
    "<c r=\"J2\"><v>6.0</v></c>",
    "<c r=\"K2\"><v>4.0</v></c>",
    "<c r=\"L2\"><v>2.0</v></c>",
    "</row>",
    "<row r=\"3\">",
    "<c r=\"A3\" t=\"inlineStr\"><is><t>CHASAPRES</t></is></c>",
    "<c r=\"B3\" t=\"inlineStr\"><is><t>J_A</t></is></c>",
    "<c r=\"C3\" t=\"inlineStr\"><is><t>d20210412</t></is></c>",
    "<c r=\"D3\"><v>4.0</v></c>",
    "<c r=\"E3\"><v>3.5</v></c>",
    "<c r=\"F3\"><v>6.0</v></c>",
    "<c r=\"G3\"><v>2.0</v></c>",
    "<c r=\"H3\"><v>4.5</v></c>",
    "<c r=\"I3\"><v>8.0</v></c>",
    "<c r=\"J3\"><v>5.5</v></c>",
    "<c r=\"K3\"><v>4.5</v></c>",
    "<c r=\"L3\"><v>3.0</v></c>",
    "</row>",
    "</sheetData></worksheet>"
  )

  # Sheet 2 (napping_coord): Produit, J1X, J1Y
  sheet2 <- paste0(
    "<worksheet><sheetData>",
    "<row r=\"1\">",
    "<c r=\"A1\" t=\"inlineStr\"><is><t>Produit</t></is></c>",
    "<c r=\"B1\" t=\"inlineStr\"><is><t>J1X</t></is></c>",
    "<c r=\"C1\" t=\"inlineStr\"><is><t>J1Y</t></is></c>",
    "</row>",
    "<row r=\"2\">",
    "<c r=\"A2\"><v>18</v></c><c r=\"B2\"><v>10</v></c><c r=\"C2\"><v>20</v></c>",
    "</row>",
    "<row r=\"3\">",
    "<c r=\"A3\"><v>17</v></c><c r=\"B3\"><v>30</v></c><c r=\"C3\"><v>40</v></c>",
    "</row>",
    "</sheetData></worksheet>"
  )

  # Sheet 3 (napping_words): Produit, J1A, J2B
  sheet3 <- paste0(
    "<worksheet><sheetData>",
    "<row r=\"1\">",
    "<c r=\"A1\" t=\"inlineStr\"><is><t>Produit</t></is></c>",
    "<c r=\"B1\" t=\"inlineStr\"><is><t>J1A</t></is></c>",
    "<c r=\"C1\" t=\"inlineStr\"><is><t>J2B</t></is></c>",
    "</row>",
    "<row r=\"2\">",
    "<c r=\"A2\"><v>17</v></c>",
    "<c r=\"B2\" t=\"inlineStr\"><is><t>amer</t></is></c>",
    "<c r=\"C2\" t=\"inlineStr\"><is><t>acide</t></is></c>",
    "</row>",
    "</sheetData></worksheet>"
  )

  # Sheet 4 (profiles): ProductName, CJ, amer (values 10 and 80 to test >10 boundary), acide (value 4)
  sheet4 <- paste0(
    "<worksheet><sheetData>",
    "<row r=\"1\">",
    "<c r=\"A1\" t=\"inlineStr\"><is><t>ProductName</t></is></c>",
    "<c r=\"B1\" t=\"inlineStr\"><is><t>CJ</t></is></c>",
    "<c r=\"C1\" t=\"inlineStr\"><is><t>amer</t></is></c>",
    "<c r=\"D1\" t=\"inlineStr\"><is><t>acide</t></is></c>",
    "</row>",
    "<row r=\"2\">",
    "<c r=\"A2\" t=\"inlineStr\"><is><t>17</t></is></c>",
    "<c r=\"B2\" t=\"inlineStr\"><is><t>J_B</t></is></c>",
    "<c r=\"C2\"><v>10</v></c>",
    "<c r=\"D2\"><v>4</v></c>",
    "</row>",
    "<row r=\"3\">",
    "<c r=\"A3\" t=\"inlineStr\"><is><t>17</t></is></c>",
    "<c r=\"B3\" t=\"inlineStr\"><is><t>J_A</t></is></c>",
    "<c r=\"C3\"><v>80</v></c>",
    "<c r=\"D3\"/>",
    "</row>",
    "</sheetData></worksheet>"
  )

  build_xlsx(
    list(sheet1, sheet2, sheet3, sheet4),
    file.path(xlsx_dir, "session.xlsx")
  )

  # Verify prepare_chasselas happy path
  out_chas <- file.path(input_dir, "out_chasselas.tsv")
  ret_chas <- prepare_chasselas(
    input_dir = input_dir,
    sessions = 1L,
    output = out_chas
  )
  expect_identical(ret_chas, out_chas)
  df_chas <- utils::read.delim(out_chas, stringsAsFactors = FALSE)
  expect_identical(
    names(df_chas),
    c("date", "jury", "product", "session", "taste", "value")
  )
  expect_identical(nrow(df_chas), 18L)
  expect_identical(unique(df_chas$jury), c("jury_01", "jury_02"))
  expect_identical(
    sort(unique(df_chas$product)),
    c("product_1before", "product_2after")
  )
  expect_identical(
    sort(unique(df_chas$taste)),
    sort(c(
      "sourness",
      "bitterness",
      "sweetness",
      "saltiness",
      "fatness, volume",
      "balance",
      "freshness",
      "persistency",
      "mouthwatering"
    ))
  )
  # J_A (jury_01) rated CHASAPRES (product_2after), J_B (jury_02) rated CHASAVANT (product_1before)
  expect_identical(
    unique(df_chas$product[df_chas$jury == "jury_01"]),
    "product_2after"
  )
  expect_identical(
    unique(df_chas$product[df_chas$jury == "jury_02"]),
    "product_1before"
  )

  # Verify prepare_profiles happy path & FIZZ >10 boundary (10 stays 10; 80 -> 8)
  out_prof <- file.path(input_dir, "out_profiles.tsv")
  ret_prof <- prepare_profiles(
    input_dir = input_dir,
    sessions = 1L,
    output = out_prof
  )
  expect_identical(ret_prof, out_prof)
  df_prof <- utils::read.delim(out_prof, stringsAsFactors = FALSE)
  expect_identical(
    names(df_prof),
    c(
      "fraction",
      "session",
      "jury",
      "taste_original",
      "taste_harmonized",
      "value"
    )
  )
  expect_identical(nrow(df_prof), 3L)
  expect_identical(df_prof$fraction, rep("fraction_17", 3L))
  expect_identical(df_prof$session, rep("session_06", 3L))
  expect_identical(df_prof$jury, c("jury_01", "jury_02", "jury_02"))
  expect_identical(df_prof$taste_harmonized, c("BITTER", "BITTER", "ACID"))
  expect_equal(df_prof$value, c(8, 10, 4), tolerance = 1e-12)

  # Verify prepare_napping happy path
  out_nap_c <- file.path(input_dir, "out_nap_coords.tsv")
  out_nap_d <- file.path(input_dir, "out_nap_desc.tsv")
  ret_nap <- prepare_napping(
    input_dir = input_dir,
    sessions = 1L,
    output_coordinates = out_nap_c,
    output_descriptors = out_nap_d
  )
  expect_identical(
    ret_nap,
    list(coordinates = out_nap_c, descriptors = out_nap_d)
  )
  df_nap_c <- utils::read.delim(out_nap_c, stringsAsFactors = FALSE)
  expect_identical(names(df_nap_c), c("fraction", "session", "J01X", "J01Y"))
  expect_identical(df_nap_c$fraction, c(17L, 18L))
  expect_identical(df_nap_c$J01X, c(30L, 10L))

  df_nap_d <- utils::read.delim(out_nap_d, stringsAsFactors = FALSE)
  expect_identical(
    names(df_nap_d),
    c(
      "fraction",
      "session",
      "jury",
      "taste_original",
      "taste_intermediate",
      "taste_harmonized"
    )
  )
  expect_identical(nrow(df_nap_d), 2L)
  expect_identical(df_nap_d$jury, c("J01A", "J02B"))
  expect_identical(df_nap_d$taste_harmonized, c("BITTER", "ACID"))
})

# 2b. Numeric CJ protection, FIZZ >100 med_fallback, and distinct NA harmonization keys
withr::with_tempdir({
  input_dir <- getwd()
  xlsx_dir <- file.path(input_dir, "20210412_cluster6", "03_files")
  dir.create(xlsx_dir, recursive = TRUE)

  td <- tempfile("xlsx_edge_")
  dir.create(file.path(td, "xl", "worksheets"), recursive = TRUE)
  writeLines(
    paste0(
      "<styleSheet><cellXfs count=\"2\">",
      "<xf numFmtId=\"0\"/><xf numFmtId=\"14\"/>",
      "</cellXfs></styleSheet>"
    ),
    file.path(td, "xl", "styles.xml")
  )
  # Sheet 1 (chasselas) with integer CJ = 1, 2 and serial Date
  writeLines(
    paste0(
      "<worksheet><sheetData>",
      "<row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>ProductName</t></is></c><c r=\"B1\" t=\"inlineStr\"><is><t>CJ</t></is></c><c r=\"C1\" t=\"inlineStr\"><is><t>Date</t></is></c><c r=\"D1\" t=\"inlineStr\"><is><t>amer_1</t></is></c></row>",
      "<row r=\"2\"><c r=\"A2\" t=\"inlineStr\"><is><t>CHASAVANT</t></is></c><c r=\"B2\"><v>1</v></c><c r=\"C2\" s=\"1\"><v>44298</v></c><c r=\"D2\"><v>3.5</v></c></row>",
      "<row r=\"3\"><c r=\"A3\" t=\"inlineStr\"><is><t>CHASAPRES</t></is></c><c r=\"B3\"><v>2</v></c><c r=\"C3\" s=\"1\"><v>44298</v></c><c r=\"D3\"><v>6.0</v></c></row>",
      "</sheetData></worksheet>"
    ),
    file.path(td, "xl", "worksheets", "sheet1.xml")
  )
  writeLines(
    "<worksheet><sheetData/></worksheet>",
    file.path(td, "xl", "worksheets", "sheet2.xml")
  )
  writeLines(
    "<worksheet><sheetData/></worksheet>",
    file.path(td, "xl", "worksheets", "sheet3.xml")
  )
  # Sheet 4 (profiles): integer CJ = 1..2, amer=6 then amer=500 -> med_fallback=6, plus two unharmonized terms
  writeLines(
    enc2utf8(paste0(
      "<worksheet><sheetData>",
      "<row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>ProductName</t></is></c><c r=\"B1\" t=\"inlineStr\"><is><t>CJ</t></is></c><c r=\"C1\" t=\"inlineStr\"><is><t>amer</t></is></c><c r=\"D1\" t=\"inlineStr\"><is><t>sans_gout</t></is></c><c r=\"E1\" t=\"inlineStr\"><is><t>neutre</t></is></c></row>",
      "<row r=\"2\"><c r=\"A2\" t=\"inlineStr\"><is><t>17</t></is></c><c r=\"B2\"><v>1</v></c><c r=\"C2\"><v>6.0</v></c><c r=\"D2\"><v>2.0</v></c><c r=\"E2\"><v>4.0</v></c></row>",
      "<row r=\"3\"><c r=\"A3\" t=\"inlineStr\"><is><t>17</t></is></c><c r=\"B3\"><v>2</v></c><c r=\"C3\"><v>500</v></c><c r=\"D3\"/><c r=\"E3\"/></row>",
      "</sheetData></worksheet>"
    )),
    file.path(td, "xl", "worksheets", "sheet4.xml"),
    useBytes = TRUE
  )
  old_wd <- setwd(td)
  utils::zip(
    file.path(xlsx_dir, "session.xlsx"),
    files = "xl",
    flags = "-rq"
  )
  setwd(old_wd)
  unlink(td, recursive = TRUE)

  # Numeric CJ stays out of 'name'/'taste' in load_session & prepare_chasselas
  load_session <- get("load_session", envir = ns)
  loaded_p <- load_session(
    input_dir,
    list(cluster = 6, date = 20210412, product_name = "CHASAVANT"),
    "profiles"
  )
  expect_true("CJ" %in% names(loaded_p))
  expect_false(any(loaded_p$name %in% c("CJ", "Date")))

  out_chas <- file.path(input_dir, "chas_num_cj.tsv")
  prepare_chasselas(input_dir = input_dir, sessions = 1L, output = out_chas)
  df_c <- utils::read.delim(out_chas, stringsAsFactors = FALSE)
  expect_identical(sort(unique(df_c$jury)), c("jury_01", "jury_02"))
  expect_identical(unique(df_c$taste), "bitterness")
  expect_identical(unique(df_c$date), "2021-04-12")

  # value = 500 uses med_fallback (6.0), and distinct unharmonized NA terms are both preserved
  custom_spec <- data.frame(
    original = c("AMER", "SANS_GOUT", "NEUTRE"),
    translated = c("BITTER", "SANS_GOUT", "NEUTRE"),
    translated_simple = c("BITTER", "SANS_GOUT", "NEUTRE"),
    stringsAsFactors = FALSE
  )
  custom_gen <- data.frame(
    original = c("BITTER", "SANS_GOUT", "NEUTRE"),
    translated = c("BITTER", NA_character_, NA_character_),
    translated_simple = c("BITTER", NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  out_prof <- file.path(input_dir, "prof_edge.tsv")
  prepare_profiles(
    input_dir = input_dir,
    sessions = 1L,
    output = out_prof,
    dictionary_generic_path = custom_gen,
    dictionary_specific_path = custom_spec
  )
  df_p <- utils::read.delim(out_prof, stringsAsFactors = FALSE)
  expect_equal(df_p$value[df_p$jury == "jury_02"], 6.0, tolerance = 1e-12)
  na_rows <- df_p[
    df_p$jury == "jury_01" &
      df_p$taste_original %in% c("SANS_GOUT", "NEUTRE"),
  ]
  expect_identical(nrow(na_rows), 2L)
})

# 3. plot_napping happy and error paths
withr::with_tempdir({
  data(napping_coordinates, package = "sapid")
  data(napping_descriptors, package = "sapid")
  coords_path <- file.path(getwd(), "coords.tsv")
  desc_path <- file.path(getwd(), "descriptors.tsv")
  utils::write.table(
    as.data.frame(napping_coordinates),
    file = coords_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  utils::write.table(
    as.data.frame(napping_descriptors),
    file = desc_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  res_nap_out <- plot_napping(
    input_coordinates = coords_path,
    input_descriptors = desc_path,
    sessions = "session_02"
  )
  expect_false(any(grepl("^Rplots", list.files(getwd()))))
  expect_true(is.list(res_nap_out))
  expect_identical(
    names(res_nap_out),
    c("pmfa", "mfa", "hcpc", "indscal")
  )
  expect_true(!is.null(res_nap_out$mfa$eig))

  nap_pdf <- file.path(getwd(), "napping_out.pdf")
  res_nap_pdf <- plot_napping(
    input_coordinates = coords_path,
    input_descriptors = desc_path,
    sessions = 2,
    output = nap_pdf
  )
  expect_false(any(grepl("^Rplots", list.files(getwd()))))
  expect_true(file.exists(nap_pdf))
  expect_true(file.info(nap_pdf)$size > 1000)
  expect_identical(rawToChar(readBin(nap_pdf, "raw", 4L)), "%PDF")
  expect_true(is.list(res_nap_pdf))

  coords_all <- as.data.frame(napping_coordinates[
    napping_coordinates$session == "session_02",
    ,
    drop = FALSE
  ])
  coords_all$session <- "session_All"
  desc_all <- as.data.frame(napping_descriptors[
    napping_descriptors$session == "session_02",
    ,
    drop = FALSE
  ])
  desc_all$session <- "session_All"
  plot_napping(
    input_coordinates = coords_all,
    input_descriptors = desc_all,
    sessions = "All"
  )

  expect_error(
    plot_napping(
      input_coordinates = napping_coordinates[1:2, , drop = FALSE],
      input_descriptors = napping_descriptors,
      sessions = 2L
    ),
    "differing number of rows"
  )
})
