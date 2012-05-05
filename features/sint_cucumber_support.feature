Feature: Support BDD with sinan
  In order to make it easier to do behaviour driven development in sinan
  As an Erlang Developer
  I want to be able to put features in a features directory in the
  project root and have those features be run by cumberl when I run a
  "cucumber" command.

  Scenario: Run cucumber on passing tests
    Given a generated project
    And a feature file in the features directory of that project
    And an implementation of that feature that passes
    When a cucumber step is run on this project
    Then then sinan should run cucumberl on the features in the features directory
    And report the build as passing

  Scenario: Run cucumber on failing tests
    Given a generated project
    And a feature file in the features directory of that project
    And an implementation of that feature that fails
    When a cucumber step is run on this project

    And report the build as failing
