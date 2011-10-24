Feature: Make the gen task of sinan more scriptable
  In order to make it easier to test sinan from sinan
  As an Erlang Developer
  I want sinan to be able to run a project gen task pragmatically,
  from erlang, without user input.

  Scenario: Run gen without user input
    Given an empty temp directory with no project
    When gen is called pragmatically with out available user input
    And a build is run
    Then sinan should build the project normally
