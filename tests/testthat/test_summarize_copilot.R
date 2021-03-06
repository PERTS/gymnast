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

modules::import(
  'dplyr',
  `%>%`,
  'arrange',
  'as_tibble',
  'filter',
  'mutate',
  'rename',
  'select',
  'tibble',
  'tribble'
)

summarize_copilot <- import_module("summarize_copilot")
sql <- import_module("sql")
util <- import_module("util")

tables <- sql$prefix_tables(list(
  user = cbind(
    tribble(
      ~uid,     ~owned_teams,          ~owned_organizations,
      'User_A', '["Team_A","Team_B"]', '[]',
      'User_B', '[]',                  '["Organization_M","Organization_N"]'
    ),
    tribble(
      ~name,         ~email,
      'Team Member', 'a@a.com',
      'Org Admin',   'b@b.com'
    )
  ),
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
    'Team_C', 'Team C', '["Organization_M"]',
    'Team_D', 'Team D', '[]'
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
    'Network_Y', 'Meta Network',   '["Network_X"]',
    'Network_Z', 'Overlapping Network', '["Organization_M", "Organization_O"]'
  )
))

describe('team_user', {
  it('default case', {
    tu <- summarize_copilot$team_user(
      tables$team,
      tables$user
    )

    expected <- tribble(
      ~team.uid, ~user.uid,
      'Team_A',  'User_A',
      'Team_B',  'User_A',
      'Team_C',  NA,
      'Team_D',  NA
    )

    expect_equal(
      tu %>% select(team.uid, user.uid),
      expected
    )
  })
})

describe('team_organization', {
  it('default case', {
    team_org <- summarize_copilot$team_organization(
      tables$team,
      tables$organization
    )

    expected <- tribble(
      ~team.uid, ~organization.uid,
      'Team_A',  'Organization_M',
      'Team_A',  'Organization_N',
      'Team_B',  'Organization_O',
      'Team_C',  'Organization_M',
      'Team_D',  NA
    )

    expect_equal(
      team_org %>% select(team.uid, organization.uid),
      expected
    )
  })
})

describe('organization_user', {
  it('default case', {
    org_user <- summarize_copilot$organization_user(
      tables$organization,
      tables$user
    )

    expected <- tribble(
      ~organization.uid, ~user.uid,
      'Organization_M',  'User_B',
      'Organization_N',  'User_B',
      'Organization_O',  NA
    )

    expect_equal(
      org_user %>% select(organization.uid, user.uid),
      expected
    )
  })
})

describe('team_organization_user', {
  it('default case', {
    tou <- summarize_copilot$team_organization_user(
      tables$team,
      tables$organization,
      tables$user
    )

    expected <- tribble(
      ~team.uid, ~organization.uid, ~organization.name, ~user.uid,
      'Team_A',  'Organization_M',  'Org M',            'User_B',
      'Team_A',  'Organization_N',  'Org N',            'User_B',
      'Team_B',  'Organization_O',  'Org O',            NA,
      'Team_C',  'Organization_M',  'Org M',            'User_B',
      'Team_D',  NA,                NA,                 NA
    )

    expect_equal(
      tou %>% select(team.uid, organization.uid, organization.name, user.uid),
      expected
    )
  })
})

describe('team_community_names', {
  it('default case', {
    comm_names <- summarize_copilot$team_community_names(
      tables$team,
      tables$organization,
      tables$user
    )

    expected <- tribble(
      ~team.uid, ~community_names, ~community_admin_names, ~community_admin_emails,
      'Team_A',  'Org M, Org N',   'Org Admin',            'b@b.com',
      'Team_B',  'Org O',          '',                     '',
      'Team_C',  'Org M',          'Org Admin',            'b@b.com',
      'Team_D',  '',               '',                     ''
    )

    expect_equal(comm_names, expected)
  })
})

describe('get_classrooms_from_organization', {
  it('default case', {
    child_assc <- summarize_copilot$get_classrooms_from_organization(
      c('Organization_M', 'Organization_N', 'Organization_O', 'Team_ignored'),
      tables$classroom,
      tables$team,
      tables$organization
    ) %>% arrange(parent_id, child_id)

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
    expected = cbind(expected1, expected2) %>%
      as_tibble() %>%
      arrange(parent_id, child_id)

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
        tables$classroom[integer(), ],
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
        tables$team[integer(), ],
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
        tables$organization[integer(), ]
      )
    )
  })

  it('stops if tables don\'t have prefixed column names', {
    expect_error(
      summarize_copilot$get_classrooms_from_organization(
        character(),
        tables$classroom %>% rename(uid = classroom.uid),
        tables$team,
        tables$organization,
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
    expected = as_tibble(cbind(expected1, expected2))

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
    expected = as_tibble(cbind(expected1, expected2))

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
        tables$classroom[integer(), ],
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
        tables$team[integer(), ],
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
        tables$organization[integer(), ],
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
        tables$network[integer(), ]
      )
    )
  })

  it('stops if tables don\'t have prefixed column names', {
    expect_error(
      summarize_copilot$get_classrooms_from_network(
        character(),
        tables$classroom,
        tables$team,
        tables$organization,
        tables$network %>% rename(uid = network.uid)
      )
    )
  })

  it('supports a single child assigned to two networks', {
    # when a single child belongs to more than one parent, all parent/child
    # combinations should be included in the final assc table
    classroom_assc <- summarize_copilot$get_classrooms_from_network(
      c('Network_X', 'Network_Z'),
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
      'Network_Z',  'Overlapping Network', 'Organization_M', 'Org M',     'Team_A',
      'Network_Z',  'Overlapping Network', 'Organization_M', 'Org M',     'Team_C',
      'Network_Z',  'Overlapping Network', 'Organization_O', 'Org O',     'Team_B'
    )
    expected2 <- tribble(
      ~team.name, ~classroom.uid, ~classroom.code,
      'Team A',   'Classroom_A',  'alpha fox',
      'Team C',   'Classroom_C',  'charlie fox',
      'Team A',   'Classroom_A',  'alpha fox',
      'Team A',   'Classroom_A',  'alpha fox',
      'Team C',   'Classroom_C',  'charlie fox',
      'Team B',   'Classroom_B',  'beta fox'
    )
    expected = as_tibble(cbind(expected1, expected2))
    print(classroom_assc)

    expect_equal(classroom_assc, expected)
  })
})

describe('map_responses_to_cycles', {
  response_tbl <- tribble(
    ~participant_id, ~created,              ~code,
    'Participant_1', '2020-01-01 12:00:00', 'trout viper', # Team Viper
    'Participant_2', '2020-01-15 12:00:00', 'bass viper', # Team Viper
    'Participant_3', '2020-01-01 12:00:00', 'fancy fox' # Team Fox
  )

  triton.cycle <- tribble(
    ~uid,       ~team_id,     ~start_date,  ~extended_end_date,    ~ordinal,
    'Cycle_1',  'Team_Viper', '2020-01-01', '2020-01-14',          1,
    'Cycle_2',  'Team_Viper', '2020-01-15', '2020-01-30',          2,
    'Cycle_3',  'Team_Fox',   '2020-01-01', '2020-01-14',          1,
    'Cycle_4',  'Team_Fox',   '2020-01-15', '2020-01-30',          2
  ) %>% util$prefix_columns('cycle')

  triton.classroom <- tribble(
    ~team_id,     ~code,
    'Team_Viper', 'trout viper',
    'Team_Viper', 'bass viper',
    'Team_Fox',   'fancy fox'
  ) %>% util$prefix_columns('classroom')

  it('handles multiple team ids', {
    actual <- summarize_copilot$map_responses_to_cycles(
      response_tbl, triton.cycle, triton.classroom)

    additional_columns <- tribble(
      ~classroom.team_id, ~created_date, ~cycle_id, ~cycle_ordinal,
      'Team_Viper',       '2020-01-01',  'Cycle_1', 1,
      'Team_Viper',       '2020-01-15',  'Cycle_2', 2,
      'Team_Fox',         '2020-01-01',  'Cycle_3', 1
    )
    expected <- cbind(response_tbl, additional_columns) %>%
      as_tibble()

    expect_equal(actual, expected)
  })

  it('prevents overwriting column: cycle_id', {
    expect_error(
      summarize_copilot$map_responses_to_cycles(
        mutate(response_tbl, cycle_id = 'foo'),
        triton.cycle,
        triton.classroom
      )
    )
  })

  it('prevents overwriting column: created_date', {
    expect_error(
      summarize_copilot$map_responses_to_cycles(
        mutate(response_tbl, created_date = 'foo'),
        triton.cycle,
        triton.classroom
      )
    )
  })

  it('prevents overwriting column: cycle_ordinal', {
    expect_error(
      summarize_copilot$map_responses_to_cycles(
        mutate(response_tbl, cycle_ordinal = 'foo'),
        triton.cycle,
        triton.classroom
      )
    )
  })
})
