Feature: Sinan should all configuration at the $HOME dir level
  In order to allow a developer to set cross project defaults (and defaults not in the project config)
  As an Erlang Developer
  I want to sinan to be able to look in the home directory for a config that provides defaults

  Scenario: Allow setting of the home directory
    Given a generated project
    And a non-standard home directory
    When a build step is run on this project
    And a home directory is specified on the command line
    Then build the app normally
    And have the home directory correctly specified in the build state

  Scenario: Allow config in the home directory
    Given a generated project
    And a non-standard home directory
    And a sinan config in that home directory
    When a build step is run on this project
    And a home directory is specified on the command line
    Then sinan uses the config in the alternate home directory
    And builds the app normally
    And have the home directory correctly specified in the build state
