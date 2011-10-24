Feature: Support automatically populating module list in app
  In order to reduce the amount of redundant information in a project
  As an Erlang Developer
  I want to sinan to be able to automatically populate the modules part
  of the app metadata

  Scenario: Have an app.src automatically populated
    Given a generated project that contains an app.src
    When a build step is run on this project
    Then build the app normally
    And sinan should put the populate the module list

  Scenario: Have an ebin/app  automatically populated
    Given a generated project that contains an ebin/app
    When a build step is run on this project
    Then build the app normally
    And sinan should put the populate the module list



