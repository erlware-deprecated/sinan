Feature: Include header files within the same app
  For modules within the same app includes via -include("X.hrl"). should work.
  (https://github.com/erlware/sinan/issues/78)

  Scenario: Build a syntem that 'includes' (not include_lib) from an include directory in the same app
    Given a generated project
    And that project an includes a header in the include directory
    And that project contains an erl file that includes that header
    When a build step is run on this project
    Then build the app normally
