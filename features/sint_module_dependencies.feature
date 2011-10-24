Feature: Correctly support module dependencies during build
  In order to correctly build complex projects
  As an Erlang Developer
  I want sinan to build all file dependencies
  before building the file that depends on them

  Scenario: Run sinan on dependent files first
    Given an erlang project that contains a behaviour
    And a module the depends on that behaviour
    When a build step is run on the project
    Then sinan should build the project correctly
