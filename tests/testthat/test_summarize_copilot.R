context("summarize_copilot.R")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the gymnast directory
# * Run this command:
#   Rscript -e "testthat::test_file('tests/testthat/test_summarize_copilot.R')"
#
# This will run all the tests in this file once. Run the command again after
# changes. Sadly, testthat::auto_test() was broken some time at or before
# version 3.0.4.

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
  'tribble',
  'pull'
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
    ~uid,       ~team_id,     ~start_date,  ~extended_end_date,    ~ordinal,  ~extended_start_date,
    'Cycle_1',  'Team_Viper', '2020-01-01', '2020-01-14',          1,         '',
    'Cycle_2',  'Team_Viper', '2020-01-15', '2020-01-30',          2,         '',
    'Cycle_3',  'Team_Fox',   '2020-01-01', '2020-01-14',          1,         '',
    'Cycle_4',  'Team_Fox',   '2020-01-15', '2020-01-30',          2,         '',
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

  it('prioritizes extended_start_date over start_date when both are present', {
    triton.cycle <- tribble(
      ~uid,       ~team_id,     ~start_date,  ~extended_end_date,    ~ordinal,  ~extended_start_date,
      'Cycle_1',  'Team_Viper', '2020-01-01', '2020-01-14',          1,         '2019-06-30',
      'Cycle_2',  'Team_Viper', '2020-01-15', '2020-01-30',          2,         ''
    ) %>% util$prefix_columns('cycle')

    response_tbl <- tribble(
      ~participant_id, ~created,              ~code,
      'Participant_1', '2019-12-31 12:00:00', 'trout viper', # Team Viper
      'Participant_2', '2020-01-15 12:00:00', 'bass viper' # Team Viper
    )

    # prove that participant_1's response falls outside the start_date, but
    # inside the extended_start_date

    p1_timestamp <- response_tbl %>%
      filter(participant_id %in% "Participant_1") %>%
      pull(created)

    cycle_start_date <- triton.cycle %>%
      filter(cycle.team_id %in% "Team_Viper",
             cycle.ordinal %in% 1) %>%
      pull(cycle.start_date)

    cycle_extended_start_date <- triton.cycle %>%
      filter(cycle.team_id %in% "Team_Viper",
             cycle.ordinal %in% 1) %>%
      pull(cycle.extended_start_date)

    # response timestamp is before cycle_start_date
    expect_true(p1_timestamp < cycle_start_date)

    # but after cycle_extended_start_date
    expect_true(p1_timestamp > cycle_extended_start_date)

    # now prove that the response is assigned to cycle 1, based on the
    # extended_start_date and NOT the start_date
    actual <- summarize_copilot$map_responses_to_cycles(
      response_tbl, triton.cycle, triton.classroom)

    p1_cycle <- actual %>%
      filter(participant_id %in% "Participant_1") %>%
      pull(cycle_ordinal)

    expect_equal(p1_cycle, 1)

  })

  it('uses start_date_extended when start_date is absent', {
    triton.cycle <- tribble(
      ~uid,       ~team_id,     ~extended_start_date, ~start_date,  ~extended_end_date, ~end_date, ~ordinal,
      'Cycle_1',  'Team_Viper', '2019-06-30',         NA,           '2020-01-14',       NA,        1,
      'Cycle_2',  'Team_Viper', NA,                   '2020-01-15', '2020-01-30',       NA,        2
    ) %>% util$prefix_columns('cycle')

    response_tbl <- tribble(
      ~participant_id, ~created,              ~code,
      'Participant_1', '2020-01-01 12:00:00', 'trout viper', # Team Viper
      'Participant_2', '2020-01-15 12:00:00', 'bass viper', # Team Viper
    )

    # now prove that the response is assigned to cycle 1, based on the
    # extended_start_date. It should NOT be uncycled, as it would have been
    # before we used the extended_start_date field.
    actual <- summarize_copilot$map_responses_to_cycles(
      response_tbl, triton.cycle, triton.classroom)

    p1_cycle <- actual %>%
      filter(participant_id %in% "Participant_1") %>%
      pull(cycle_ordinal)

    expect_equal(p1_cycle, 1)

  })

})

describe('strip_token', {
  it('converts to lower case', {
    expect_identical(summarize_copilot$strip_token('ABC123'), 'abc123')
  })

  it('strips spaces in all locations', {
    expect_identical(summarize_copilot$strip_token('  a  b  '), 'ab')
  })

  it('strips unusual whitespace: newlines, tabs', {
    # \u00A0 is a non-breaking space
    expect_identical(summarize_copilot$strip_token('\ta\rb\u00A0\n'), 'ab')
  })

  it('handles characters with no obvious case: chinese, emoji', {
    # \u5c06 chinese ideograph related to Mahjong.
    expect_identical(summarize_copilot$strip_token('a\u5c06🔥b'), 'ab')
  })

  it('handles a typical email address', {
    expect_identical(
      summarize_copilot$strip_token('Student.001@school.edu'),
      'student001schooledu'
    )
  })
})

describe('recently_modified_cycles', {
  report_date <- '2021-01-16'
  time_lag_threshold <- 9

  recently_modified_all <- dplyr::tibble(
    cycle.team_id = c('Team_A', 'Team_A', 'Team_A'),
    cycle.ordinal = c(1, 2, 3),
    cycle.start_date = c('2021-01-01', '2021-01-15', '2021-01-30'),
    cycle.modified = c('2021-01-10 06:00:00','2021-01-10 06:00:00','2021-01-10 06:00:00')
  )

  all_future <- dplyr::tibble(
    cycle.team_id = c('Team_B', 'Team_B', 'Team_B'),
    cycle.ordinal = c(1, 2, 3),
    cycle.start_date = c('2021-02-01', '2021-02-15', '2021-02-30'),
    cycle.modified = c('2021-01-10 06:00:00','2021-01-10 06:00:00','2021-01-10 06:00:00')
  )

  on_report_date_modified <- dplyr::tibble(
    cycle.team_id = c('Team_C'),
    cycle.ordinal = c(1),
    cycle.start_date = report_date,
    cycle.modified = c('2021-01-10 06:00:00')
  )

  none_modified <- dplyr::tibble(
    cycle.team_id = c('Team_D', 'Team_D', 'Team_D'),
    cycle.ordinal = c(1, 2, 3),
    cycle.start_date = c('2021-01-01', '2021-01-15', '2021-01-30'),
    # old modified timestamps
    cycle.modified = c('2020-01-10 06:00:00','2020-01-10 06:00:00','2020-01-10 06:00:00')
  )

  # E has ended, F is current, G is future.
  single_cycle_teams_all_modified <- dplyr::tibble(
    cycle.team_id = c('Team_E', 'Team_F', 'Team_G'),
    cycle.ordinal = c(1, 1, 1),
    cycle.start_date = c('2021-01-01', '2021-01-15', '2021-01-30'),
    cycle.end_date = c('2021-01-10', '2021-01-20', '2021-02-05'),
    cycle.modified = c('2021-01-10 06:00:00','2021-01-10 06:00:00','2021-01-10 06:00:00')
  )

  it('excludes future cycles', {
    triton.cycle <- all_future
    team_ids <- summarize_copilot$recently_modified_cycles(
      triton.cycle,
      report_date,
      time_lag_threshold
    )

    expect_equal(length(team_ids), 0)
  })

  it('excludes cycles starting on report date', {
    triton.cycle <- on_report_date_modified
    team_ids <- summarize_copilot$recently_modified_cycles(
      triton.cycle,
      report_date,
      time_lag_threshold
    )

    expect_equal(length(team_ids), 0)
  })

  it('excludes unmodified cycles', {
    triton.cycle <- none_modified
    team_ids <- summarize_copilot$recently_modified_cycles(
      triton.cycle,
      report_date,
      time_lag_threshold
    )

    expect_equal(length(team_ids), 0)
  })

  it('includes past (ended) and current modified cycles', {
    triton.cycle <- single_cycle_teams_all_modified
    team_ids <- summarize_copilot$recently_modified_cycles(
      triton.cycle,
      report_date,
      time_lag_threshold
    )

    expect_equal(team_ids, c('Team_E', 'Team_F'))
  })

  it('returns a unique list of team ids', {
    triton.cycle <- recently_modified_all
    team_ids <- summarize_copilot$recently_modified_cycles(
      triton.cycle,
      report_date,
      time_lag_threshold
    )

    # Two cycles match, but they're on the same team, so we should get one id.
    expect_equal(team_ids, c('Team_A'))
  })
})

describe('recently_modified_rosters', {
  # Given these parameters, a "recently modified" entity will be one whose
  # timestamp is on or after 2021-01-07 00:00:00.
  report_date <- '2021-01-16'
  time_lag_threshold <- 9

  # Fields in the participant table we won't use:
  # * participant.uid
  # * participant.short_uid
  # * participant.created
  # * participant.student_id
  # * participant.stripped_student_id
  # * participant.classroom_ids
  # * participant.in_target_group
  # * participant.verified

  none_modified <- dplyr::tibble(
    participant.team_id = c('Team_A', 'Team_A', 'Team_A'),
    participant.modified = c(
      '2021-01-06 23:59:59', # last second before threshold
      '2021-01-05 06:00:00',
      '2021-01-04 06:00:00'
    ),
  )

  some_modified <- dplyr::tibble(
    participant.team_id = c('Team_B', 'Team_B', 'Team_C', 'Team_D'),
    participant.modified = c(
      '2021-01-07 00:00:00', # threshold exactly, thus recent
      '2021-01-08 06:00:00', # after threshold, thus recent
      '2021-01-08 06:00:00', # after threshold, thus recent
      '2021-01-04 06:00:00' # not recent
    ),
  )

  it('excludes unmodified participants', {
    triton.participant <- none_modified
    team_ids <- summarize_copilot$recently_modified_rosters(
      triton.participant,
      report_date,
      time_lag_threshold
    )

    expect_equal(length(team_ids), 0)
  })

  it('includes modified participants', {
    triton.participant <- some_modified
    team_ids <- summarize_copilot$recently_modified_rosters(
      triton.participant,
      report_date,
      time_lag_threshold
    )

    # Despite multiple participants in these teams, the list should be unique.
    expect_equal(team_ids, c('Team_B', 'Team_C'))
  })
})

describe('recently_created_orgs', {
  # Given these parameters, a "recently created" entity will be one whose
  # timestamp is on or after 2021-01-07 00:00:00.
  report_date <- '2021-01-16'
  time_lag_threshold <- 9

  none_created <- dplyr::tibble(
    organization.uid = c('Organization_A', 'Organization_A', 'Organization_A'),
    organization.created = c(
      '2021-01-06 23:59:59', # last second before threshold
      '2021-01-05 06:00:00',
      '2021-01-04 06:00:00'
    ),
  )

  some_created <- dplyr::tibble(
    organization.uid = c('Organization_B', 'Organization_B', 'Organization_C', 'Organization_D'),
    organization.created = c(
      '2021-01-07 00:00:00', # threshold exactly, thus recent
      '2021-01-08 06:00:00', # after threshold, thus recent
      '2021-01-08 06:00:00', # after threshold, thus recent
      '2021-01-04 06:00:00' # not recent
    ),
  )

  it('excludes unmodified orgs', {
    triton.organization <- none_created
    org_ids <- summarize_copilot$recently_created_orgs(
      triton.organization,
      report_date,
      time_lag_threshold
    )

    expect_equal(length(org_ids), 0)
  })

  it('includes modified orgs', {
    triton.organization <- some_created
    org_ids <- summarize_copilot$recently_created_orgs(
      triton.organization,
      report_date,
      time_lag_threshold
    )

    # Despite multiple participants in these teams, the list should be unique.
    expect_equal(org_ids, c('Organization_B', 'Organization_C'))
  })
})
