Feature: Make sinan build faster
  In order to make sinan build faster and to reduce file churn
  As an Erlang Developer
  I want sinan to only save its durable state when sinan terminates

  Scenario: Have sinan save its durable state only on termination
    Given a generated project
    When a build step is run on this project
    Then sinan should build the app normally
    And not save intermediate state
    And save state to a single file on close


