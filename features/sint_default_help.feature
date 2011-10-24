Feature: Make sinan more approachable
  In order to make sinan much easier to start using
  As an Erlang Developer
  I want to make sinan help the default command instead of sinan build

  Scenario: Have sinan display help if no command is given
    Given a generated project
    When a step is run on the project with an empty arglist
    Then sinan should run normally
    And display a list of commands and help usage



