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

modules::import("dplyr", `%>%`, 'filter', 'tibble', 'tribble')

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
      tables$team,
      tables$organization
    )

    expected1 <- tribble(
      ~parent_id,        ~parent_name, ~child_id, ~child_name, ~team.uid,
      'Organization_M',  'Org M',      'Team_A',  'Team A',    'Team_A',
      'Organization_M',  'Org M',      'Team_C',  'Team C',    'Team_C',
      'Organization_N',  'Org N',      'Team_A',  'Team A',    'Team_A',
      'Organization_O',  'Org O',      'Team_B',  'Team B',    'Team_B',
    )
    expected2 <- tribble(
      ~team.name, ~classroom.uid, ~classroom.code,
       'Team A',   'Classroom_A',  'alpha fox',
       'Team C',   'Classroom_C',  'charlie fox',
       'Team A',   'Classroom_A',  'alpha fox',
       'Team B',   'Classroom_B',  'beta fox'
    )
    expected = cbind(expected1, expected2)

    expect_equal(child_assc, expected)
  })

  it('returns zero rows if no ru ids found', {
    child_assc <- summarize_copilot$get_classrooms_from_organization(
      c('Organization_DNE', 'Team_ignored'),
      tables$classroom,
      tables$team,
      tables$organization
    )

    expected <- tibble(
      parent_id = character(),
      parent_name = character(),
      child_id = character(),
      child_name = character(),
      team.uid = character(),
      team.name = character(),
      classroom.uid = character(),
      classroom.code = character()
    )

    expect_equal(child_assc, expected)
  })

  it('warns if classroom table is empty', {
    expect_warning(
      summarize_copilot$get_classrooms_from_organization(
        character(),
        filter(tables$classroom, classroom.uid %in% 'does not exist'),
        tables$team,
        tables$organization
      )
    )
  })

  it('warns if team table is empty', {
    expect_warning(
      summarize_copilot$get_classrooms_from_organization(
        character(),
        tables$classroom,
        filter(tables$team, team.uid %in% 'does not exist'),
        tables$organization
      )
    )
  })

  it('warns if organization table is empty', {
    expect_warning(
      summarize_copilot$get_classrooms_from_organization(
        character(),
        tables$classroom,
        tables$team,
        filter(tables$organization, organization.uid %in% 'does not exist')
      )
    )
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

    # alpha fox listed TWICE, b/c it must be included in two different
    # network children: Org M and N.
    expected1 <- tribble(
      ~parent_id,   ~parent_name,     ~child_id,        ~child_name, ~team.uid,
      'Network_X',  'Simple Network', 'Organization_M', 'Org M',     'Team_A',
      'Network_X',  'Simple Network', 'Organization_M', 'Org M',     'Team_C',
      'Network_X',  'Simple Network', 'Organization_N', 'Org N',     'Team_A',
    )
    expected2 <- tribble(
      ~team.name, ~classroom.uid, ~classroom.code,
      'Team A',   'Classroom_A',  'alpha fox',
      'Team C',   'Classroom_C',  'charlie fox',
      'Team A',   'Classroom_A',  'alpha fox'
    )
    expected = cbind(expected1, expected2)

    expect_equal(classroom_assc, expected)
  })

  it('meta network', {
    classroom_assc <- summarize_copilot$get_classrooms_from_network(
      c('Network_Y', 'Team_ignored'),
      tables$classroom,
      tables$team,
      tables$organization,
      tables$network
    )

    # alpha fox only listed once, although a member via both Org M and N.
    expected1 <- tribble(
      ~parent_id,   ~parent_name,   ~child_id,   ~child_name,      ~team.uid,
      'Network_Y',  'Meta Network', 'Network_X', 'Simple Network', 'Team_A',
      'Network_Y',  'Meta Network', 'Network_X', 'Simple Network', 'Team_C',
    )
    expected2 <- tribble(
      ~team.name, ~classroom.uid, ~classroom.code,
      'Team A',   'Classroom_A',  'alpha fox',
      'Team C',   'Classroom_C',  'charlie fox'
    )
    expected = cbind(expected1, expected2)

    expect_equal(classroom_assc, expected)
  })


  it('returns zero rows if no ru ids found', {
    child_assc <- summarize_copilot$get_classrooms_from_network(
      c('Network_DNE', 'Team_ignored'),
      tables$classroom,
      tables$team,
      tables$organization,
      tables$network
    )

    expected <- tibble(
      parent_id = character(),
      parent_name = character(),
      child_id = character(),
      child_name = character(),
      team.uid = character(),
      team.name = character(),
      classroom.uid = character(),
      classroom.code = character()
    )

    expect_equal(child_assc, expected)
  })

  it('warns if classroom table is empty', {
    expect_warning(
      summarize_copilot$get_classrooms_from_network(
        character(),
        filter(tables$classroom, classroom.uid %in% 'does not exist'),
        tables$team,
        tables$organization,
        tables$network
      )
    )
  })

  it('warns if team table is empty', {
    expect_warning(
      summarize_copilot$get_classrooms_from_network(
        character(),
        tables$classroom,
        filter(tables$team, team.uid %in% 'does not exist'),
        tables$organization,
        tables$network
      )
    )
  })

  it('warns if organization table is empty', {
    expect_warning(
      summarize_copilot$get_classrooms_from_network(
        character(),
        tables$classroom,
        tables$team,
        filter(tables$organization, organization.uid %in% 'does not exist'),
        tables$network
      )
    )
  })

  it('warns if network table is empty', {
    expect_warning(
      summarize_copilot$get_classrooms_from_network(
        character(),
        tables$classroom,
        tables$team,
        tables$organization,
        filter(tables$network, network.uid %in% 'does not exist')
      )
    )
  })
})
