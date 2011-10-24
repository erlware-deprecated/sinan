Feature: sinan gen should support generating *.app.src instead of ebin/app
  In order to make a sinan generated project 'correct'
  As an Erlang Developer
  I want sinan to generate application metadata into src/<appname>.app.src
  instead of ebin/<appname>.app

  Scenario: Generate app.src into the src directory
    Given an empty temp directory with no project
    When the sinan gen task is called
    And a build is run
    Then sinan should generate an app.src into the src directory
    And build the project normally
