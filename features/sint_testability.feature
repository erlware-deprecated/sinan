Feature: Make sinan more testable
  In order to make it easier to test sinan from sinan
  As an Erlang Developer
  I want sinan to be able to pass the start directory to a sinan
  call instead of having it inferred.

  Scenario: Pass the start dir to a sinan project
    Given a generated project in a different location then the CWD
    When a build step is run on this project
    And a start dir is passed to the build
    Then sinan should build the project in the location specified by the start dir
