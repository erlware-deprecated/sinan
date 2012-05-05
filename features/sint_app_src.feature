Feature: Support app.src files in the src directory
  In order to more obvious what are actually source files (any kind of source file)
  As an Erlang Developer
  I want to be able to put an <app-name>.app.src file in my src directory
  and have it be copied into the correct place

  Scenario: Have an app.src handled correctly when no ebin exists
    Given a generated project that contains an app.src
    And does not contain an ebin/app
    When a build step is run on this project
    Then sinan should put the app file in ebin/.app
    And build the app normally

  Scenario: Have an ebin/app handled correctly when no app.src exists
    Given a generated project that contains an ebin/app
    And does not contain an app.src
    When a build step is run on this project
    Then sinan should put the app file in ebin/.app
    And build the app normally


