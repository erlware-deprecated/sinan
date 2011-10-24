Feature: Sinan does not correctly parse_transform dependencies
  In order to ensure sinan correctly builds projects with parse transforms
  As an Erlang Developer
  I want to sinan to be able to be able to detect the existance of a parse transform
  and build the modules in the correct order

  Scenario: Build a parse_transform included project in the correct order
    Given a generated project
    And that project contains a parse transform
    And that project contains a module that depends on that parse transform
    When a build step is run on this project
    Then build the app normally
