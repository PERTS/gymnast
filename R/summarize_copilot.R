modules::import(
  "dplyr",
  `%>%`,
  "arrange",
  "between",
  "distinct",
  "filter",
  "first",
  "group_by",
  "left_join",
  "mutate",
  "n",
  "pull",
  "rename",
  "right_join",
  "select",
  "summarise",
  "tibble"
)
modules::import("lubridate", "ymd")
modules::import("stringr", "str_pad")
modules::import("tidyr", "spread")

json_utils <- import_module("json_utils")
logging <- import_module("logging")
perts_ids <- import_module("perts_ids")
util <- import_module("util")

`%+%` <- paste0

merge_participants <- function(triton.participant, neptune.participant) {
  # Args are rows directly from db tables, with prefixed column names.
  # names(triton.participant) %in% c('participant.uid', 'participant.in_target_group', ...)
  # names(neptune.participant) %in% c('participant.uid', 'participant.name', ...)
  #
  # Returns df with names 'participant_id' and 'in_target_group' where the
  # latter is %in% c(0, 1, NA).

  if (!all(triton.participant$in_target_group %in% c(0, 1))) {
    stop("Triton participant target group values not all in (0, 1).")
  }

  # Rename some triton columns for clarity. Things get pretty confusing since
  # both tables are named "participant".
  triton_for_join <- triton.participant %>%
    select(
      triton.stripped_student_id = participant.stripped_student_id,
      triton.team_id = participant.team_id,
      in_target_group = participant.in_target_group
    )

  neptune.participant %>%
    left_join(
      triton_for_join,
      by = c(
        participant.organization_id = "triton.team_id",
        participant.name = "triton.stripped_student_id"
      )
    ) %>%
    select(participant.uid, in_target_group) %>%
    rename(participant_id = participant.uid)
}

team_class <- function(triton.program,
                       triton.team,
                       triton.classroom,
                       triton.user) {
  # Merge all useful data at team-class level.
  captain <- triton.user %>%
    select(
      captain.uid = user.uid,
      captain.name = user.name,
      captain.email = user.email
    )
  contact <- triton.user %>%
    select(
      contact.uid = user.uid,
      contact.name = user.name,
      contact.email = user.email
    )
  # team-class
  tc <- triton.team %>%
    left_join(triton.program, by = c(team.program_id = "program.uid")) %>%
    left_join(captain, by = c(team.captain_id = "captain.uid")) %>%
    left_join(triton.classroom, by = c(team.uid = "classroom.team_id")) %>%
    left_join(contact, by = c(classroom.contact_id = "contact.uid"))

  return(tc)
}

team_cycle_class <- function(team_class, triton.cycle) {
  # Join cycles.
  left_join(team_class, triton.cycle, by = c(team.uid = "cycle.team_id"))
}

team_cycle_class_participation <- function(tcc,
                                           triton.cycle,
                                           triton.classroom,
                                           neptune.participant,
                                           neptune.participant_data) {
  if (nrow(neptune.participant_data) == 0) {
    logging$warning("No participation data!")
    tcc$num_completed_by_pd <- 0
    return(tcc)
  }

  # Join participation "ppn" tables `participant` and `participant_data`.
  # Note the direction of the join which means that participants with no data
  # won't appear here.
  # Returns team-cycle-class level data, with added cols:
  # * num_completed_by_pd ("pd" means from the participant_data table)
  ppn <- neptune.participant_data %>%
    left_join(
      neptune.participant,
      by = c(participant_data.participant_id = "participant.uid")
    ) %>%
    # We'll use the neptune participant data table like the saturn response
    # table, which means we'll need a `code` and `created` column.
    # To make it look like a response table, make sure there's a `code` column.
    mutate(
      code = participant_data.code,
      created = participant_data.modified
    )

  # WARNING about using the ":cycle-X" in the survey_id column:
  # These are NOT reliable indicators of which cycle the participation should
  # be allocated to because cycle dates can change, while this value is set
  # at the time of participation. Furthermore, participation that occurs
  # outside of any cycle does not receive the ":cycle-X" suffix, but
  # nevertheless may later be legitimately be counted as cycle participation if
  # cycles dates in copilot change to encompass it.

  # Assign a cycle ordinal to each response row, one team at a time, since
  # different teams will have different cycle dates and different numbers of
  # cycles.
  pd_w_cycle <- NULL
  for (team_id in unique(tcc$team.uid)) {
    team_pd <- filter(ppn, participant.organization_id %in% team_id)
    team_cycle <- filter(triton.cycle, cycle.team_id %in% team_id)
    team_class <- filter(triton.classroom, classroom.team_id %in% team_id)

    if (nrow(team_pd) == 0 || nrow(team_class) == 0) {
      # Either no participation, no classrooms, or both.
      next
    }

    to_bind <- map_responses_to_cycles(team_pd, team_cycle, team_class)
    if (is.null(pd_w_cycle)) {
      pd_w_cycle <- to_bind
    } else {
      pd_w_cycle <- rbind(pd_w_cycle, to_bind)
    }
  }

  # code-cycle (or classroom-cycle) level participation
  code_cycle_ppn <- pd_w_cycle %>%
    group_by(code, cycle_ordinal) %>%
    summarise(num_completed_by_pd = n())

  # team-cycle-class level participation
  tcc_ppn <- tcc %>%
    left_join(
      code_cycle_ppn,
      by = c(classroom.code = "code", cycle.ordinal = "cycle_ordinal")
    )

  return(tcc_ppn)
}

team_organization <- function(triton.team, triton.organization) {
  triton.team %>%
    json_utils$expand_string_array_column(team.organization_ids) %>%
    rename(organization.uid = "team.organization_ids") %>%
    # team-org level
    left_join(triton.organization, by = 'organization.uid')
}

organization_user <- function(triton.organization, triton.user) {
  triton.user %>%
    json_utils$expand_string_array_column(user.owned_organizations) %>%
    rename(organization.uid = user.owned_organizations) %>%
    right_join(triton.organization, by = 'organization.uid')

}

team_organization_user <- function(triton.team,
                                   triton.organization,
                                   triton.user) {
  admin_assc <- organization_user(
    triton.organization %>% select(organization.uid),
    triton.user
  ) %>%
    # Drop users who aren't admin on any organizations, otherwise users
    # associated with org "NA" will get joined to teams associated with org "NA"
    # which is wrong.
    filter(!is.na(user.uid))

  team_organization(triton.team, triton.organization) %>%
    left_join(admin_assc, by = 'organization.uid')
}

team_community_names <- function(triton.team,
                                 triton.organization,
                                 triton.user) {
  # Summarize the names of the communities associated with teams.
  # Returns df with cols:
  # * team.uid
  # * community_names
  # * community_admin_names
  # * community_admin_emails
  tou <- team_organization_user(
    triton.team,
    triton.organization,
    triton.user
  )

  if (all(is.na(tou$organization.uid))) {
    # There are no communities to process. Add the expected columns as NA.
    return(triton.team %>%
      select(team.uid) %>%
      mutate(
        community_names = NA,
        community_admin_names = NA,
        community_admin_emails = NA
      )
    )
  }

  tou %>%
    group_by(team.uid) %>%
    summarise(
      community_names = paste(
        unique(ifelse(is.na(organization.name), "", organization.name)),
        collapse = ", "
      ),
      community_admin_names = paste(
        unique(ifelse(is.na(user.name), "", user.name)),
        collapse = ", "
      ),
      community_admin_emails = paste(
        unique(ifelse(is.na(user.email), "", user.email)),
        collapse = ", "
      )
    )
}

format_cycle_dates <- function(start_dates, end_dates) {
  # Summarize date ranges of all cycles as a single string.
  if (length(start_dates) != length(end_dates)) {
    stop("summarize_copilot$format_cycle_dates: lengths don't match.")
  }

  # Create alphabetically-ordered cycle names based on start dates.
  start_dates_posix <- ymd(start_dates)
  end_dates_posix <- ymd(end_dates)

  cycle_dates <- paste0(
    "Cycle ",
    str_pad(sequence(length(start_dates)), 2, pad = "0"),
    " (",
    ifelse(is.na(start_dates_posix), "NA   ", strftime(start_dates_posix, "%m/%d")),
    " - ",
    ifelse(is.na(end_dates_posix), "NA   ", strftime(end_dates_posix, "%m/%d")),
    ")"
  )
  return(cycle_dates)
}

cycle_dates <- function(triton.team, triton.cycle) {
  # Summarize a team's cycle settings and where cycles fall relative to today.
  # Returns df with cols:
  # * team.uid
  # * cycle_dates
  # * cycles_past
  # * cycle_present
  # * cycles_future

  # "past present future": use a comparison function to filter a vector of
  # cycle ordinals and return matches as a comma-separated list, e.g. '2, 3'.
  ppf <- function(compare) {
    function(start_dates, end_dates, ordinals) {
      is_scheduled <- is.character(start_dates) & is.character(end_dates)
      # %in% TRUE changes NAs to FALSE
      matches <- (is_scheduled & compare(start_dates, end_dates)) %in% TRUE
      return(paste(ordinals[matches], collapse = ", "))
    }
  }

  # Comparison functions for ppf
  today <- strftime(Sys.Date(), "%Y-%m-%d")
  past <- function(start_d, end_d) end_d < today
  present <- function(start_d, end_d) end_d >= today & start_d <= today
  future <- function(start_d, end_d) start_d > today

  # Strip cycle prefixes for brevity.
  names(triton.cycle) <- gsub("^cycle\\.", "", names(triton.cycle))
  # Order so displays look nice.
  triton.cycle <- arrange(triton.cycle, team_id, ordinal)

  triton.team %>%
    left_join(triton.cycle, by = c(team.uid = "team_id")) %>%
    group_by(team.uid) %>%
    summarise(
      cycle_dates = paste(
        format_cycle_dates(start_date, end_date),
        collapse = "; "
      ),
      cycles_past = ppf(past)(start_date, end_date, ordinal),
      cycle_present = ppf(present)(start_date, end_date, ordinal),
      cycles_future = ppf(future)(start_date, end_date, ordinal)
    )
}

map_responses_to_cycles <- function(response_tbl,
                                    triton.cycle,
                                    triton.classroom) {
  # Tag each row with the appropriate cycle, based on date. Could be used to
  # tag pd or survey responses.
  #
  # response_tbl must be indexed by participant response and have colum `code`.
  # Note that both neptune.participant_data and saturn.response have these
  # properties.
  #
  # triton.cycle and triton.classroom come directly from the triton db, but must
  # be filtered to one team.
  #
  # Returns response level data with added cols:
  # * classroom.*
  # * cycle_ordinal

  bad_class_teams <- length(unique(triton.classroom$classroom.team_id)) > 1
  bad_cycle_teams <- length(unique(triton.cycle$cycle.team_id)) > 1
  if (bad_class_teams | bad_cycle_teams) {
    stop("Classes or cycles not filtered correctly.")
  }
  missing_codes <- unique(
    response_tbl$code[!response_tbl$code %in% triton.classroom$classroom.code]
  )
  if (length(missing_codes) > 0) {
    logging$info(
      "These participation codes don't appear in Copilot, most likely from",
      "deleted classrooms: ",
      missing_codes
    )
  }

  response_merged <- response_tbl %>%
    left_join(triton.classroom, by = c(code = "classroom.code")) %>%
    mutate(
      # Convert datetimes to dates; char > POSIXlt > char
      # This will allow us to compare to cycle dates.
      created_date = created %>%
        strptime("%Y-%m-%d %H:%M:%S") %>%
        strftime("%Y-%m-%d"),
      cycle_ordinal = NA # Populated below.
    )

  # Fill in values of the cycle_ordinal column as their row matches various
  # cycle dates.
  for (i in sequence(nrow(triton.cycle))) {
    this_cycle <- triton.cycle[i, ]
    in_cycle <- (
      response_merged$created_date >= this_cycle$cycle.start_date &
        response_merged$created_date <= this_cycle$cycle.end_date
    )
    response_merged$cycle_ordinal[in_cycle] <- this_cycle$cycle.ordinal
  }

  response_merged
}

get_started_ids <- function(team_cycle_class_participation, threshold) {
  # Returns character of class ids meeting the threshold in any cycle.

  if (!identical(between(threshold, 0, 100), TRUE)) {
    stop(paste0("Threshold must be between 0 and 100. Got: ", threshold))
  }

  max_class_ppn <- team_cycle_class_participation %>%
    mutate(ratio_surveyed = num_completed_by_pd / classroom.num_students) %>%
    group_by(classroom.uid) %>%
    summarise(max_surveyed = max(ratio_surveyed, na.rm = TRUE))

  if (threshold == 0) {
    # Threshold zero means ANY participation counts, but not NONE.
    started <- filter(max_class_ppn, max_surveyed > 0)
  } else {
    # Any other value is inclusive, e.g. 80% meets a threshold of 80.
    started <- filter(max_class_ppn, max_surveyed >= threshold / 100)
  }

  return(started$classroom.uid)
}

cycle_participation_wide <- function(team_cycle_class_participation) {
  # Create a spread/cast df of participation rates for each cycle within
  # each team. Includes absolute counts and rates as a percent of the
  # total rostered students within that cycle.
  #
  # Besides decimals from 0 to 1.0, values can also be:
  # * '(not yet)' - for cycles in the future so ppn can't have happened
  # * '(dates not set)' - for cycle w/o dates set
  # * '' - when a cycle with this ordinal doesn't exist for this team
  #
  # Example:
  # | team.uid | cycle.pct.1 |   cycle.pct.2   | cycle.pct.3 | avg.pct |
  # |----------|-------------|-----------------|-------------|---------|
  # | Team_ABC | 67%         | 50%             |          0% |     39% |
  # | Team_DEF | (not yet)   | (dates_not_set) |             |      0% |

  team_ordinal_grouped <- team_cycle_class_participation %>%
    filter(!is.na(cycle.ordinal)) %>%
    rename(cycle = cycle.ordinal) %>%
    group_by(team.uid, cycle, cycle.start_date)

  today <- strftime(Sys.Date(), "%Y-%m-%d")
  percent <- team_ordinal_grouped %>%
    summarise(
      num = sum(num_completed_by_pd, na.rm = TRUE),
      denom = sum(classroom.num_students, na.rm = TRUE)
    ) %>%
    mutate(
      pct = ifelse(
        is.na(cycle.start_date),
        "(dates not set)",
        ifelse(
          cycle.start_date > today,
          "(not yet)",
          paste0(util$clean_percent(num, denom), '%')
        )
      ),
      # Not for display, but for further calculations re: averages.
      pct_numeric = util$clean_percent(num, denom)
    )

  percent_wide <- percent %>%
    select(team.uid, cycle, pct, -cycle.start_date, -pct_numeric) %>%
    spread(cycle, pct, sep = ".pct.")

  percent_avg <- percent %>%
    group_by(team.uid) %>%
    summarise(avg.pct = round(mean(pct_numeric)))

  absolute_wide <- team_ordinal_grouped %>%
    summarise(num = sum(num_completed_by_pd, na.rm = TRUE)) %>%
    select(-cycle.start_date) %>%
    spread(cycle, num, sep = ".abs.")

  cycle_ppn_wide <- percent_wide %>%
    left_join(percent_avg, by = "team.uid") %>%
    left_join(absolute_wide, by = "team.uid")

  return(cycle_ppn_wide)
}

map_responses_to_cycles_orig <- function(response_data_merged, cycle_tbl) {
  # Old code from the metascript, cleaned up somewhat by CAM on 2020-03-04.

  logging$debug("########## MAP CYCLES TO RESPONSES ##########")
  # Here we attempt to map all survey responses to cycles in the cycle_tbl
  # (using extended end dates).
  # Successfully-mapped responses get a cycle_name.
  # Unmapped responses are expected to fall into two categories:
  ### 1 - Responses made in a team that has never defined ANY cycles.
  ### These responses get the generic label "(no cycles defined)".
  ### 2 - Responses made before the start of a team's first cycle.
  ### These responses are CUT!


  # Helper function for assigning dates to cycle_names.
  cycle_from_date_iter <- function(date, team_id, cycle_tbl) {
    # given an input date, an associated team_id, and a cycle_tbl,
    # return the name of the first cycle that contains the date for that team.
    # if team id is not in cycle_tbl, return "(no cycles defined)".
    # if no cycle is matched, return NA.
    if (!team_id %in% unique(cycle_tbl$team_id)) {
      return("(no cycles defined)")
    }
    cycle_tbl_team <- cycle_tbl[cycle_tbl$team_id %in% team_id, ]
    cycle_tbl_team$in_cycle <- Vectorize(between)(
      date,
      cycle_tbl_team$start_date,
      cycle_tbl_team$end_date_x
    )
    if (!any(cycle_tbl_team$in_cycle)) {
      return(NA)
    }
    in_cycle <- first(which(cycle_tbl_team$in_cycle))
    return(unlist(cycle_tbl_team[in_cycle, "cycle_name"]))
  }
  cycle_for_date <- Vectorize(cycle_from_date_iter, c("date", "team_id"))


  # Try to map all responses to cycles. Unmapped responses get NA for cycle_name.
  response_data_cycles_base <- response_data_merged
  response_data_cycles_base$StartDate_formatted <- response_data_cycles_base$StartDate %>%
    # POSIXlt, including time
    strptime("%Y-%m-%d %H:%M:%S") %>%
    # drop time, just keep date
    as.Date()
  response_data_cycles_base$cycle_name <- cycle_for_date(
    response_data_cycles_base$StartDate_formatted,
    response_data_cycles_base$team_id,
    cycle_tbl
  )

  # Tag rows that have pre-first-cycle data.
  first_start_date_for_teams <- cycle_tbl %>%
    group_by(team_id) %>%
    summarise(first_cycle_start_date = first(start_date))

  response_data_cycles_merged <- left_join(
    response_data_cycles_base,
    first_start_date_for_teams,
    by = "team_id"
  )

  has_start_date <- !is.na(response_data_cycles_merged$first_cycle_start_date)
  pre_first_cycle <- (
    response_data_cycles_merged$StartDate_formatted <
      response_data_cycles_merged$first_cycle_start_date
  )
  response_data_cycles_merged$cycle_name <- ifelse(
    has_start_date & pre_first_cycle,
    "(pre-first-cycle)",
    response_data_cycles_merged$cycle_name
  )

  # Sanity-check: ALL ROWS should be either tagged with a cycle, tagged as being
  # from a team with no cycles defined, or tagged as being pre-first-cycle. No NAs.
  if (any(is.na(response_data_cycles_merged$cycle_name))) {
    logging$warning(
      "Some rows in the data were NOT successfully tagged as being part " %+%
        "of a cycle, before a first cycle, or from a team that created no " %+%
        "cycles. They are NA rows and will be cut. Consider investigating " %+%
        "further."
    )
  }

  return(response_data_cycles_merged)
}

get_classrooms_from_organization <- function(organization_ids,
                                             triton.classroom,
                                             triton.team,
                                             triton.organization) {
  platform_tables <- list(triton.classroom, triton.team, triton.organization)
  for (table in platform_tables) {
    if (nrow(table) == 0) {
      warning(
        "summarize_copilot$get_classrooms_from_organization() received " %+%
        "a platform table with zero rows."
      )
    }
  }

  team_org_assoc <- triton.team %>%
    json_utils$expand_string_array_column(team.organization_ids) %>%
    rename(organization.uid = "team.organization_ids") %>%
    filter(organization.uid %in% organization_ids) %>%
    left_join(triton.organization, by = 'organization.uid')

  team_org_class <- team_org_assoc %>%
    left_join(triton.classroom, by = c(team.uid = 'classroom.team_id'))

  team_org_class %>%
    mutate(
      parent_id = organization.uid,
      parent_name = organization.name,
      child_id = team.uid,
      child_name = team.name
    ) %>%
    select(
      parent_id,
      parent_name,
      child_id,
      child_name,
      team.uid,
      team.name,
      classroom.uid,
      classroom.code
    )
}

get_classrooms_from_network <- function (network_ids,
                                         triton.classroom,
                                         triton.team,
                                         triton.organization,
                                         triton.network) {
  platform_tables <- list(
    triton.classroom,
    triton.team,
    triton.organization,
    triton.network
  )
  for (table in platform_tables) {
    if (nrow(table) == 0) {
      warning(
        "summarize_copilot$get_classrooms_from_organization() received " %+%
        "a platform table with zero rows."
      )
    }
  }

  # Long form relationship table, unique by network-child
  network_assc <- triton.network %>%
    filter(network.uid %in% network_ids) %>%
    json_utils$expand_string_array_column(network.association_ids)

  # Structure of df to return.
  classroom_assc <- tibble(
    parent_id = character(),
    parent_name = character(),
    child_id = character(),
    child_name = character(),
    team.uid = character(),
    team.name = character(),
    classroom.uid = character(),
    classroom.code = character(),
  )

  for (x in sequence(length(network_assc$network.uid))) {
    network_id <- network_assc$network.uid[x]
    network_name <- network_assc$network.name[x]
    child_id <- network_assc$network.association_ids[x]
    child_kind <- perts_ids$get_kind(child_id)

    if (child_kind %in% 'Organization') {
      cl <- get_classrooms_from_organization(
        child_id,
        triton.classroom,
        triton.team,
        triton.organization
      )

      to_add <- cl %>%
        mutate(child_id = parent_id, child_name = parent_name) %>%
        mutate(parent_id = network_id, parent_name = network_name)
    } else if (child_kind %in% 'Network') {
      # recurse
      child_assc <- get_classrooms_from_network(
        child_id,
        triton.classroom,
        triton.team,
        triton.organization,
        triton.network
      )

      child_name <- triton.network %>%
        filter(network.uid == child_id) %>%
        pull(network.name)

      to_add <- child_assc %>%
        mutate(
          parent_id = network_id,
          parent_name = network_name,
          # !! to reference the variable, not the column name
          child_id = !!child_id,
          child_name = !!child_name
        )
    }

    classroom_assc <- rbind(classroom_assc, to_add)
  }

  distinct(classroom_assc, child_id, classroom.uid, .keep_all = TRUE)
}
