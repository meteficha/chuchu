# Of course it needs calc!
@needs_calc
@nice_feature
Feature: Division
  In order to avoid silly mistakes
  Cashiers must be able to calculate a fraction

  Scenario: Regular numbers
    Given that I have entered 2 into the calculator
    # Comment! =)
    And that I have entered 3 into the calculator
    When I press divide
    Then the result should be 1 on the screen
