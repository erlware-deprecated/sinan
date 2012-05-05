Feature: Support Multiple Concurrent Runs in Sinan
  In order to make it easier to do test sinan
  As a Sinan Developer
  I want to be able to run multiple instances of sinan concurrently

  Scenario: Run sinan twice correctly
    Given two generated projects
    When a build step is run on each project concurrently
    Then sinan should build both projects without a problem
