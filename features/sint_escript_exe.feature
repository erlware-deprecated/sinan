Feature: Support generating an escript from a project
  In order to make it easier to distribute certain kinds of projects
  As an Erlang Developer
  I want to be able to generate an escript from my project

  Scenario: Generate a complex escript
    Given a generated project
    And a project name named module with a main function
    When an escript step is run on this project
    Then build the project normally
    And produce an executable escript

  Scenario: Generate a complex escript with script
    Given a generated project
    And a escript directive in the build config
    And a escript file that should become the base
    When an escript step is run on this project
    Then build the project normally
    And produce an executable escript



