CREATE OR REPLACE VIEW trips_kpis AS
SELECT e.ssn, first_name, last_name, dept, (2017 - start_year) AS working_years,
CASE dept WHEN 200 THEN "Finance"
						WHEN 201 THEN "Operations"
						WHEN 202 THEN "IT"
						WHEN 203 THEN "Quality"
						WHEN 204 THEN "Sales"
						WHEN 205 THEN "Purchasing"
						WHEN 206 THEN "Logistics"
						WHEN 207 THEN "HR"
						WHEN 208 THEN "Executive"
END AS dept_name,
 total_trips, total_expense, total_reimbursement,
 ROUND((total_expense - total_reimbursement), 2) AS due_to_reimburse
FROM employees e
	JOIN 	(	SELECT employee,ROUND(SUM(gross_amount),2) AS total_expense
				FROM expenses  
				GROUP BY employee
			) AS expenses_total
		ON e.ssn = expenses_total.employee
	JOIN 	(	SELECT employee, COUNT(start_date) AS total_trips
				FROM trips 
				GROUP BY employee
			) AS trips_total
		ON e.ssn = trips_total.employee
	JOIN 	(	SELECT employee,ROUND(SUM(reimbursement_amount),2) AS total_reimbursement
				FROM reimbursements 
				GROUP BY employee
			) AS reimbursement_total
		ON e.ssn = reimbursement_total.employee;
	
-- question2
SELECT * FROM trips_kpis;

-- Top 5 departments with the biggest total expense
SELECT dept_name, total_department_expense
FROM
	(SELECT dept_name, (ROUND(SUM(total_expense), 2)) AS total_department_expense
	FROM trips_kpis
	GROUP BY dept_name) AS top_5
ORDER BY total_department_expense DESC LIMIT 5;

-- Top 20 employees with the biggest total expense and their average expense per trip
SELECT first_name, last_name, dept_name, total_expense, 
ROUND((total_expense/total_trips), 2) AS avg_expense_per_trip
FROM trips_kpis
ORDER BY total_expense DESC LIMIT 20;

-- Total amount due to reimburse
SELECT ROUND(SUM(due_to_reimburse), 2) AS total_due_to_reimburse
FROM trips_kpis;

-- Top 5 departments with the biggest average expense per trip
SELECT t1.dept_name,
       ROUND(t1.total_department_expense / t2.total_department_trips, 2) AS avg_expense_per_trip
FROM
  (SELECT dept_name, 
          ROUND(SUM(total_expense), 2) AS total_department_expense
   FROM trips_kpis
   GROUP BY dept_name) AS t1
JOIN
  (SELECT dept_name, 
          SUM(total_trips) AS total_department_trips
   FROM trips_kpis
   GROUP BY dept_name) AS t2
ON t1.dept_name = t2.dept_name
ORDER BY avg_expense_per_trip DESC LIMIT 5;

