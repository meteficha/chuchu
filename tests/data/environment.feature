Feature: Environment
  In order to use the enviroment variables
  Programmers must be able to write and read environment variables

  Scenario: Write then read
    When I set the variable as 3 into the environment
    # Comment! =)
    Then the variable should have 3 on its content
