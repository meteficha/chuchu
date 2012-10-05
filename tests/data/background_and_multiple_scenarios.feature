Feature: A Background and Multiple scenarios

  This feature has a background that must be executed before the
  execution of each scenario. Both scenarios succeed when executed
  individually and must succeed when executed together.

  However, due to a bug in chuchu (<= 0.1.2), the test fails if both
  scenarios are executed together because chuchu only executes the
  background on the beginning of the test and not before each
  scenario.


  Background: Bathroom's door is open
    Given that the bathroom's door is open

  Scenario: Anybody can use the bathroom
    Given that Bob entered the bathroom and locked the door

  Scenario: Anybody can use the bathroom
    Given that Alice entered the bathroom and locked the door
