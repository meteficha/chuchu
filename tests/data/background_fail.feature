Feature: When background fails, scenario shouldn't be executed.

  Background:
    Given that this step will fail

  Scenario:
    Then this step should not be executed
