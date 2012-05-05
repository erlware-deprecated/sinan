Feature: Sinan should support building joxa source
  In order to ensure sinan correctly builds joxa
  As an Erlang Developer
  I want to sinan to be able to be able to build joxa source

  Scenario: Build joxa source
    Given a generated project that contains joxa modules
    When a build step is run on this project
    Then build the app normally
