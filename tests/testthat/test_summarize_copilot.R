context("summarize_copilot.R")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the gymnast directory
# * Run this command:
#   Rscript -e "testthat::auto_test('R', 'tests/testthat')"
#
# The test runner will watch your code files and re-run your tests when you
# change something. Get all those yummy green checks!

if (grepl("tests/testthat$", getwd())) {
  setwd("../..")  # root dir of gymnast repo
}

modules::import(
  'dplyr',
  'tibble'
)
summarize_copilot <- import_module("summarize_copilot")

describe("get_classrooms_from_network", {
  it("handles simple networks of organizations", {
    network_tbl <- tibble(
      uid = "Network_foo",
      name = "Foo Net",
      association_ids = '["Organization_foo"]'
    )

    org_tbl <- tibble(
      uid = "Organization_foo",
      name = "Org Foo"
    )

    team_tbl <- tibble(
      uid = "Team_foo",
      name = "Team Foo",
      organization_ids = '["Organization_foo"]'
    )

    class_tbl <- tibble(
      uid = "Classroom_foo",
      name = "Classroom Foo",
      team_id = "Team_foo",
      code = "trout viper"
    )

    classrooms <- summarize_copilot$get_classrooms_from_network(
      "Network_foo",
      class_tbl,
      team_tbl,
      org_tbl,
      network_tbl
    )

    # It should find the one classroom that is associated from the tbl df.
    expect_identical(classrooms$uid, class_tbl$uid)
    expect_identical(classrooms$code, class_tbl$code)
  })

  it("handles meta-networks", {
    network_tbl <- tibble(
      uid =             c("Network_foo",          "Network_meta"),
      name =            c("Foo Net",              "Meta Net"),
      association_ids = c('["Organization_foo"]', '["Network_foo"]')
    )

    org_tbl <- tibble(
      uid = "Organization_foo",
      name = "Org Foo"
    )

    team_tbl <- tibble(
      uid = "Team_foo",
      name = "Team Foo",
      organization_ids = '["Organization_foo"]'
    )

    class_tbl <- tibble(
      uid = "Classroom_foo",
      name = "Classroom Foo",
      team_id = "Team_foo",
      code = "trout viper"
    )

    classrooms <- summarize_copilot$get_classrooms_from_network(
      "Network_meta",
      class_tbl,
      team_tbl,
      org_tbl,
      network_tbl
    )

    # It should find the one classroom that is associated from the tbl df,
    # even though the relation is via another network.
    expect_identical(classrooms$uid, class_tbl$uid)
    expect_identical(classrooms$code, class_tbl$code)
  })

  it("handles sets of networks", {
    network_tbl <- tibble(
      uid =             c("Network_foo",          "Network_meta",    "Network_bar"),
      name =            c("Foo Net",              "Meta Net",        "Bar Net"),
      association_ids = c('["Organization_foo"]', '["Network_foo"]', '["Organization_bar"]')
    )

    org_tbl <- tibble(
      uid = c("Organization_foo", "Organization_bar"),
      name = c("Org Foo",         "Org Bar")
    )

    team_tbl <- tibble(
      uid =              c("Team_foo",             "Team_bar"),
      name =             c("Team Foo",             "Team Bar"),
      organization_ids = c('["Organization_foo"]', '["Organization_bar"]')
    )

    class_tbl <- tibble(
      uid =     c("Classroom_foo", "Classroom_bar", "Classroom_other"),
      name =    c("Classroom Foo", "Classroom Bar", "Classroom Other"),
      team_id = c("Team_foo",      "Team_bar",      "Team_other"),
      code =    c("trout viper",   "evil eel",      "indifferent ibis")
    )

    classrooms <- summarize_copilot$get_classrooms_from_network(
      network_tbl$uid,
      class_tbl,
      team_tbl,
      org_tbl,
      network_tbl
    )

    print("!!!!")
    print(classrooms)
    print(class_tbl[1:2]$uid)

    # Note that "Classroom Foo" is in two different networks ("foo" and "meta")
    # but it should only appear in the output once.
    expect_identical(classrooms$uid, class_tbl[1:2, ]$uid)
    expect_identical(classrooms$code, class_tbl[1:2, ]$code)
  })
})
