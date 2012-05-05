Feature: Remove no longer useful helpers
  In order to make sinan code less redundant
  As an Sinan Developer
  I want sinan do not want sinan to generate the erlware helper and bin script

  Scenario: Have sinan not generate the erlware helper and bin script
    Given an empty directory
    When a project is generated
    Then sinan should not generate the erlware helper
    And should not generate the exe script


