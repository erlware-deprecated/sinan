Feature: Support dependency configuration
  In order to allow the erlang user more control over individual dependencies
  As a Sinan User
  I want to be able to put a set of version specifications in my sinan.config
  and have sinan use those configurations to drive the dependencies

  Scenario: Have sinan parse dependencies specifications
    Given a generated project that contains a dependency spec
    When a build step is run on this project
    Then sinan builds the app normally
    And correctly figures out the constrained dependencies

  Scenario: Have sinan parse dependencies specifications, for releases
    Given a generated project that contains a dependency spec
    And has multiple releases
    When a build step is run on this project on each release
    Then sinan builds the app normally each time
    And correctly figures out the constrained dependencies for each release




