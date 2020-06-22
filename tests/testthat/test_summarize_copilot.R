context("summarize_copilot.R")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the rserve directory
# * Run this command:
#   Rscript -e "testthat::auto_test('R', 'tests/testthat')"
#
# The test runner will watch your code files and re-run your tests when you
# change something. Get all those yummy green checks!

if (grepl("tests/testthat$", getwd())) {
  setwd("../..")  # root dir of gymnast repo
}

library(testthat)

modules::import("dplyr", `%>%`, 'tribble')

bootstrap <- modules::use("R/bootstrap.R")
bootstrap$install_module_imports()

summarize_copilot <- import_module("summarize_copilot")
sql <- import_module("sql")

tables <- sql$prefix_tables(list(
  classroom = tribble(
    ~uid,          ~name,     ~team_id, ~code,      
    'Classroom_A', 'Class A', 'Team_A', 'alpha fox',
    'Classroom_B', 'Class B', 'Team_B', 'beta fox',
    'Classroom_C', 'Class C', 'Team_C', 'charlie fox'
  ),
  team = tribble(
    ~uid,     ~name,    ~organization_ids,
    'Team_A', 'Team A', '["Organization_M", "Organization_N"]',
    'Team_B', 'Team B', '["Organization_O"]',
    'Team_C', 'Team C', '["Organization_M"]'
  ),
  organization = tribble(
    ~uid,             ~name,
    'Organization_M', 'Org M',
    'Organization_N', 'Org N',
    'Organization_O', 'Org O'
  ),
  network = tribble(
    ~uid,        ~name,            ~association_ids,
    'Network_X', 'Simple Network', '["Organization_M", "Organization_N"]',
    'Network_Y', 'Meta Network',   '["Network_X"]'
  )
))

describe('get_classrooms_from_organization', {
  it('default case', {
    child_assc <- summarize_copilot$get_classrooms_from_organization(
      c('Organization_M', 'Organization_N', 'Organization_O', 'Team_ignored'),
      tables$classroom,
      tables$team
    )

    expected <- tribble(
      ~organization.uid, ~team.uid, ~classroom.uid, ~classroom.code,
      'Organization_M',  'Team_A',  'Classroom_A',  'alpha fox',
      'Organization_M',  'Team_C',  'Classroom_C',  'charlie fox',
      'Organization_N',  'Team_A',  'Classroom_A',  'alpha fox',
      'Organization_O',  'Team_B',  'Classroom_B',  'beta fox'
    )

    expect_equal(child_assc, expected)
  })
})

describe('get_classrooms_from_network', {
  it('simple network', {
    classroom_assc <- summarize_copilot$get_classrooms_from_network(
      c('Network_X', 'Team_ignored'),
      tables$classroom,
      tables$team,
      tables$organization,
      tables$network
    )

    expected <- tribble(
      ~network.uid, ~child_id,        ~child_name, ~classroom.uid, ~classroom.code,
      # alpha fox listed TWICE, b/c it must be included in two different
      # network children: Org M and N.
      'Network_X',  'Organization_M', 'Org M',     'Classroom_A',  'alpha fox',
      'Network_X',  'Organization_M', 'Org M',     'Classroom_C',  'charlie fox',
      'Network_X',  'Organization_N', 'Org N',     'Classroom_A',  'alpha fox'
    )

    expect_equal(classroom_assc, expected)
  })

  it('meta network', {
    # print(tables$network)
    classroom_assc <- summarize_copilot$get_classrooms_from_network(
      c('Network_Y', 'Team_ignored'),
      tables$classroom,
      tables$team,
      tables$organization,
      tables$network
    )

    print(classroom_assc)

    expected <- tribble(
      ~network.uid, ~child_id,   ~child_name,      ~classroom.uid, ~classroom.code,
      # alpha fox only listed once, although a member via both Org M and N.
      'Network_Y',  'Network_X', 'Simple Network', 'Classroom_A',  'alpha fox',
      'Network_Y',  'Network_X', 'Simple Network', 'Classroom_C',  'charlie fox'
    )

    expect_equal(classroom_assc, expected)
  })
})
