libname chinook 'C:\Users\rsmadja\Desktop\SQL Individual assignement\Chinook dataset-20200922';
/*FINANCIAL*/
/*Total sales of Chinook*/
Proc  sql; 
Select sum(total) as Global_total_sales format =DOLLAR13.2
from chinook.invoices;
Quit;
/*Sales per year*/
proc sql; 
Select year(datepart(invoicedate)) as Year,sum(total) as Global_total_sales_per_year format =DOLLAR13.2
from chinook.invoices
group by year
order by year asc;
quit;
/*Sales by months in 2009*/
proc sql; 
Select month(datepart(invoicedate)) as Month,year(datepart(invoicedate)) as Year,sum(total) as Global_total_sales_per_month
from chinook.invoices
where year(datepart(invoicedate))=2009
group by month, year
order by month asc;
quit;
/*Quantity sold in best month-January 2009*/
Proc sql;
Select sum(quantity) as quantity_sold_in_best_month
	from chinook.invoice_items as y, 
		chinook.invoices as i
where y.invoiceid=i.invoiceid and
year(datepart(invoicedate))=2009 and 
month(datepart(invoicedate))=1; 
Quit;
/*quantity sold in worst month-November 2011*/
Proc sql;
Select sum(quantity) as quantity_sold_in_best_month
	from chinook.invoice_items as y, 
		chinook.invoices as i
where y.invoiceid=i.invoiceid and
year(datepart(invoicedate))=2011 and 
month(datepart(invoicedate))=11; 
Quit;
/*What artists bought during the best month-January 2009?*/
proc sql;
Select a.name as artists_sold_the_best_month
from chinook.artists as a, 
chinook.albums as l,
chinook.tracks as t, 
chinook.invoice_items as y, 
chinook.invoices as i
where  l.artistid=a.artistid and
	l.albumid=t.albumid and
	t.trackid=y.trackid and
	i.invoiceid=y.invoiceid and
	year(datepart(invoicedate))=2009 and 
	month(datepart(invoicedate))=1; 
	Quit;
/*How many buy of ACDC tracks during the best month-January 2009*/
proc sql;
Select count (a.name) as AC_DC
from
(Select a.name
from chinook.artists as a, 
chinook.albums as l,
chinook.tracks as t, 
chinook.invoice_items as y, 
chinook.invoices as i
where  l.artistid=a.artistid and
	l.albumid=t.albumid and
	t.trackid=y.trackid and
	i.invoiceid=y.invoiceid and
	year(datepart(invoicedate))=2011 and 
	month(datepart(invoicedate))=11)
where a.name like 'AC/DC'; 
quit;
/*Albums bought during the best month-January 2009*/
proc sql;
Select l.title
from chinook.albums as l, 
chinook.tracks as t, 
chinook.invoice_items as y, 
chinook.invoices as i
where l.albumid=t.albumid and
	t.trackid=y.trackid and
	i.invoiceid=y.invoiceid and
	year(datepart(invoicedate))=2009 and 
	month(datepart(invoicedate))=1; 
	quit;
/*Sales by month in 2010*/
proc sql; 
Select month(datepart(invoicedate)) as Month,year(datepart(invoicedate)) as Year,sum(total) as Global_total_sales_per_month
from chinook.invoices
where year(datepart(invoicedate))=2010
group by month, year
order by month asc;
quit;
/*Sales by month in 2011*/
proc sql; 
Select month(datepart(invoicedate)) as Month,year(datepart(invoicedate)) as Year,sum(total) as Global_total_sales_per_month
from chinook.invoices
where year(datepart(invoicedate))=2011
group by month, year
order by month asc;
quit;
/*Sales by month in 2012*/
proc sql; 
Select month(datepart(invoicedate)) as Month,Year(datepart(invoicedate)) as Year,sum(total) as Global_total_sales_per_month
from chinook.invoices
where year(datepart(invoicedate))=2012
group by month, year
order by month asc;
quit;
/*Sales by month in 2013*/
proc sql; 
Select month(datepart(invoicedate)) as Month,Year(datepart(invoicedate)) as Year,sum(total) as Global_total_sales_per_month
from chinook.invoices
where year(datepart(invoicedate))=2013
group by month, year
order by month asc;
quit;
/*Average number of items per invoice*/
proc sql; 
Select int(avg(number_of_items)) as average_items_per_invoice
from (Select sum(quantity) as number_of_items,invoiceid 
From chinook.invoice_items
group by invoiceid);
quit;
/*Median of the number if items per invocie*/
proc sql; 
Select int(median(number_of_items)) as median_items_per_invoice
from (Select sum(quantity) as number_of_items,invoiceid 
From chinook.invoice_items
group by invoiceid);
quit;


/*CUSTOMERS*/
/*Number of good customers in terms of sales (more than average sales per customers*/
Proc sql;
Select count(Type) as number_of_good_customers
from(
Select distinct c.customerid,avg(total) as average_amount_spent,
case when avg(total)> 9.307335 then 'Good Customers'
else 'Bad Customers' end as Type 
from chinook.invoice_items as y,
	chinook.invoices as i,
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
	group by c.customerid)
where type like 'Good Customers';
	quit;

Proc sql;
/*Number of Bad customers in terms of sales (Less than average sales per customers*/
Select count(Type) as number_of_Bad_customers
from(
Select distinct c.customerid,avg(total) as average_amount_spent,
case when avg(total)> 9.307335 then 'Good Customers'
else 'Bad Customers' end as Type 
from chinook.invoice_items as y,
	chinook.invoices as i,
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
	group by c.customerid)
where type like 'Bad Customers';
	quit;

/*Calculation of the average_total spent by customer*/
proc sql;
Select avg(average_price) format =DOLLAR13.2 as Average_amount_spent_by_customer
from (Select avg(total) as average_price, c.customerid
from chinook.invoice_items as y,
	chinook.invoices as i,
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
group by c.customerid);
quit;

/*Check the number of customers in the database*/
Proc sql; 
Select count (customerid) as number_of_customers
from chinook.customers; 
quit;
/*Check with a a left join if all customers are customers, in other words, if they are rencenced in the tables invoices*/
proc sql; 
Select count ( distinct c.customerid) as number_of_customers_clients
from chinook.customers as c 
left outer join chinook.invoices as i 
on c.customerid=i.customerid;
quit;
/*other way to solve it*/
 /*customers with orders - they all have orders*/
proc sql;
select count(*) as Customers_with_orders
   from (select Customerid
            from chinook.invoices
         intersect 
         select Customerid
           from chinook.customers);
quit;
/*Average recency total*/
proc sql;
select round(avg(recency)) as average_recency_in_years
from(Select int((today()-max(datepart(invoicedate)))/365,25) as recency
from chinook.invoices);
Quit;
/*Total Average items bought per customers*/
proc sql; 
Select int(avg(Sum_quantity)) as average_items_per_customer
from (Select sum(quantity) as Sum_quantity
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
group by c.customerid);
quit;
/* Calculate average products sold per customer*/
proc sql;
Select sum(quantity) as Sum_quantity,c.customerid
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
group by c.customerid;
quit;

/*Number of good customers in terms of quantity bought*/
proc sql; 
Select count(Type) as number_of_good_customers
from (Select distinct c.customerid, 
case when sum(quantity)>
(Select avg(Sum_quantity) as average_items_per_customer
from (Select sum(quantity) as Sum_quantity
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
group by c.customerid)) then 'Good customers'
else 'Bad Customers' end as Type
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid)
where type like 'Good customers';
quit;

/*Classification bad or good customers in terms of quantity bought - with the subquery method*/
proc sql; 
Select count(Type) as number_of_bad_customers
from (Select distinct c.customerid, 
case when sum(quantity)>
(Select avg(Sum_quantity) as average_items_per_customer
from (Select sum(quantity) as Sum_quantity
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
group by c.customerid)) then 'Good customers'
else 'Bad Customers' end as Type
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid)
where type like 'Bad Customers';
quit;

/*Classification bad or good customers in terms of quantity bought - with the number method*/
	proc sql; 
Select count(Type) as number_of_good_customers
from (Select distinct c.customerid, 
case when sum(quantity)> 37.9661 then 'Good customers'
else 'Bad Customers' end as Type
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid)
where type like 'Good customers'
;
quit;
/*Average quantity per customer*/
proc sql;
Select avg(Sum_quantity) as average_items_per_customer
from (Select sum(quantity) as Sum_quantity
From chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
group by c.customerid);
quit;
/*Tenure-how long are the customers with the company, make their first purchase*/
Proc sql;
select round(avg(tenure)) as average_tenure
from(
Select int((today()-min(datepart(invoicedate)))/365,25) as tenure, c.customerid
from chinook.invoice_items as y, 
	chinook.invoices as i, 
	chinook.customers as c
where y.invoiceid=i.invoiceid and
	i.customerid=c.customerid
group by c.customerid );
quit;
/*Where the customer come from?*/
Proc sql; 
Select country, count(distinct c.customerid) as number_of_customers,sum(total) as amount_spent format =DOLLAR13.2
from chinook.customers as c, 
	chinook.invoices as i
where i.customerid=c.customerid
group by Country 
order by 2 desc; 
quit;

/*Sales per continent/market*/
proc sql; 
Select sum(total) as sales format=DOLLAR13.2,
case when country='USA' or 
	country='Canada' or 
	country='Brazil' or 
	country='Chile' or 
	country="Argentina" 
	then 'America'
	when country='France' or
	country='Germany' or
	country='United Kingdom' or
	country='Czech Republic' or
	country='portugal' or
	country='Hungary' or
	country='Ireland' or 
	country='Austria' or
	country='Finland' or
	country='Netherlands' or 
	country='Norway' or
	country='Sweden' or
	country='Spain' or
	country='Belgium' or
	country='Denmark' or
	country='Italy' or
	country='Poland'
	then 'Europe'
	else 'Asia' end as Market
from chinook.customers as c, 
	chinook.invoices as i
where i.customerid=c.customerid
group by Market 
order by 1 desc; 
quit;

/*Internal Business process*/
/*Quantity sold by genre*/
proc sql; 
select g.name as genre, sum(y.quantity) as quantity_sold
from chinook.genres as g,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where g.genreid=t.genreid and
	t.trackid=y.trackid
group by g.name
order by 2 desc;
Quit;
/*5 most sold genres*/
proc sql
outobs=5; 
select g.name as genre, sum(y.quantity) as quantity_sold
from chinook.genres as g,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where g.genreid=t.genreid and
	t.trackid=y.trackid
group by g.name
order by 2 desc;
Quit;
/*avg quantity sold by genre*/
proc sql;
select int (avg(quantity_sold)) as average_quantity_sold_per_genre
from(select g.name as genre, sum(y.quantity) as quantity_sold
from chinook.genres as g,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where g.genreid=t.genreid and
	t.trackid=y.trackid
group by g.name);
quit;
/*Good performing group - quantity is more than average per genre*/
proc sql; 
select g.name as genre, sum(y.quantity) as quantity_sold
from chinook.genres as g,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where g.genreid=t.genreid and
	t.trackid=y.trackid
group by g.name
having quantity_sold > ALL (select avg(quantity_sold) as average_quantity_sold_per_genre
from(select g.name as genre, sum(y.quantity) as quantity_sold
from chinook.genres as g,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where g.genreid=t.genreid and
	t.trackid=y.trackid
group by g.name))
order by 2 desc;
Quit;
/*Bad performing group - quantity is less than average per genre*/
proc sql; 
select g.name as genre, sum(y.quantity) as quantity_sold
from chinook.genres as g,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where g.genreid=t.genreid and
	t.trackid=y.trackid
group by g.name
having quantity_sold < ALL (select avg(quantity_sold) as average_quantity_sold_per_genre
from(select g.name as genre, sum(y.quantity) as quantity_sold
from chinook.genres as g,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where g.genreid=t.genreid and
	t.trackid=y.trackid
group by g.name))
order by 2 desc;
Quit;
/*Media type and quantity sold*/
proc sql;
select m.name as media_type, sum(y.quantity) as quantity_sold
from chinook.media_types as m,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where m.mediatypeid=t.mediatypeid and
	t.trackid=y.trackid
group by m.name
order by 2 desc;
quit;
/*Media type sold the most - more than average*/
proc sql; 
select m.name as Good_performing_genre, sum(y.quantity) as quantity_sold
from chinook.media_types as m,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where m.mediatypeid=t.mediatypeid and
	t.trackid=y.trackid
group by m.name
having quantity_sold > ALL (select avg(quantity_sold) as average_quantity_sold_per_genre
from(select m.name as media_type, sum(y.quantity) as quantity_sold
from chinook.media_types as m,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where m.mediatypeid=t.mediatypeid and
	t.trackid=y.trackid
group by m.name))
order by 2 desc;
Quit;

/*Media type sold the least - less than average*/
proc sql; 
select m.name as Bad_Performing_group, sum(y.quantity) as quantity_sold
from chinook.media_types as m,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where m.mediatypeid=t.mediatypeid and
	t.trackid=y.trackid
group by m.name
having quantity_sold < ALL (select avg(quantity_sold) as average_quantity_sold_per_genre
from(select m.name as media_type, sum(y.quantity) as quantity_sold
from chinook.media_types as m,
	chinook.tracks as t, 
	chinook.invoice_items as y 
where m.mediatypeid=t.mediatypeid and
	t.trackid=y.trackid
group by m.name))
order by 2 desc;
Quit;
/*Tracks more sold - more than average*/
proc sql; 
select t.name as tracks, sum(y.quantity) as quantity_sold
from chinook.tracks as t, 
	chinook.invoice_items as y 
where t.trackid=y.trackid
group by t.name
having quantity_sold > ALL (select avg(quantity_sold) 
from (select t.name as tracks, sum(y.quantity) as quantity_sold
from chinook.tracks as t, 
	chinook.invoice_items as y 
where t.trackid=y.trackid
group by t.name));
Quit;
/*Tracks with zero sales - no results for this query*/
proc sql;
select t.name as tracks, sum(y.quantity) as quantity_sold
from chinook.tracks as t, 
	chinook.invoice_items as y 
where t.trackid=y.trackid 
group by t.name
having quantity_sold= 0;
quit;
/*Number of tracks*/
proc sql;
select count (distinct trackid) as total_number_of_tracks
from chinook.tracks;
quit;
/*tracks who don't sell well*/
proc sql;
select count (distinct t.name) as tracks_sold_only_once
from (select t.name as tracks, sum(y.quantity) as quantity_sold
from chinook.tracks as t, 
	chinook.invoice_items as y 
where t.trackid=y.trackid 
group by t.name
having quantity_sold=1);
quit;

/*Check if the artist has a link with the tracks that are not selling well*/
proc sql; 
select count(distinct artistid) as total_number_of_artists
from chinook.artists;
quit;
/*number of artists who have sold tracks only one 1 track*/
proc sql;
Select count (distinct artistid) as artists_of_Bad_tracks
from (select t.name as tracks, l.artistid
from chinook.tracks as t, 
	chinook.invoice_items as y, 
	chinook.artists as a, 
	chinook.albums as l
where t.trackid=y.trackid and
	a.artistid=l.artistid and
	t.albumid=l.albumid
group by t.name
having sum(quantity) = (Select avg(quantity_sold) from(select sum(y.quantity) as quantity_sold
from chinook.tracks as t, 
	chinook.invoice_items as y 
where t.trackid=y.trackid
group by t.name
having quantity_sold=1 )));
quit;
/*tracks not selling well and the artist related to the song*/
proc sql;
select t.name as tracks, l.artistid
from chinook.tracks as t, 
	chinook.invoice_items as y, 
	chinook.artists as a, 
	chinook.albums as l
where t.trackid=y.trackid and
	a.artistid=l.artistid and
	t.albumid=l.albumid
group by t.name
having sum(quantity) = (Select avg(quantity_sold) from(select sum(y.quantity) as quantity_sold
from chinook.tracks as t, 
	chinook.invoice_items as y 
where t.trackid=y.trackid
group by t.name
having quantity_sold=1))
order by 2 asc;
quit;

/*Albums of tracks that don't sell well*/
Proc sql;
select distinct a.albumid
from (Select a.albumid
from chinook.albums as a,
	chinook.tracks as t,
		chinook.invoice_items as y
where a.albumid=t.albumid and
	t.trackid=y.trackid 
group by t.trackid
having sum(y.quantity)=1);
Quit;
	
/*How many bytes in total for those tracks who where sold just once*/
proc sql; 
select sum(bytes) FORMAT= COMMAX20.0 as total_space
from chinook.tracks; 
quit;
/*Place saved by removing the tracks sold only once*/
proc sql;
select sum(Bytes_removed) FORMAT= COMMAX20.0 as place_saved from
(Select sum(t.bytes) as Bytes_removed
from chinook.tracks as t, 
	chinook.invoice_items as y 
where t.trackid=y.trackid 
group by t.name
having sum(quantity)=1);
quit;
/*Employees*/
/*employees about to retire*/
Proc sql;
select employeeid, title,int((today()-datepart(birthdate))/365.25) as age, 
int((today()-(datepart(hiredate)))/365.25) as time_in_the_company
from chinook.employees
where ((today()-datepart(birthdate))/365.25)>60;
quit;

/*number of different roles in the company*/
proc sql; 
select title, count (title) as number_of_roles
from chinook.employees
group by title;
quit;
/*Sales per employees*/
proc sql;
Select  employeeid, sum(total) FORMAT=DOLLAR13.2 as sales 
from chinook.employees as e, 
	chinook.customers as c, 
	chinook.invoices as i
where e.employeeid=c.supportrepid and
	c.customerid=i.customerid 
group by employeeid
order by 2 desc;
quit;
/*Employees that do not support any customers*/
proc sql;
Select EmployeeId as Employees_ID_not_sales
from chinook.employees
except
select SupportRepId
from chinook.customers;
Quit;
/*Mean of sales per employee*/
proc sql;
Select avg(sales) FORMAT=DOLLAR13.2 as Average_sales_per_employee
from(Select  employeeid, sum(total) as sales
from chinook.employees as e, 
	chinook.customers as c, 
	chinook.invoices as i
where e.employeeid=c.supportrepid and
	c.customerid=i.customerid 
group by employeeid);
quit;

/*Countries where the employee number 3 has made sales*/
Proc sql; 
select sum(total) FORMAT=DOLLAR13.2 as Sales_per_employee_3, c.country
from chinook.employees as e, 
	chinook.customers as c, 
	chinook.invoices as i
where e.employeeid=c.supportrepid and
	c.customerid=i.customerid and
	e.employeeid=3
group by employeeid, c.country
order by 1 desc; 
quit;
/*Countries where the employee number 4 has made sales*/
Proc sql; 
select sum(total) FORMAT=DOLLAR13.2 as Sales_per_employee_4, c.country
from chinook.employees as e, 
	chinook.customers as c, 
	chinook.invoices as i
where e.employeeid=c.supportrepid and
	c.customerid=i.customerid and
	e.employeeid=4
group by employeeid, c.country
order by 1 desc; 
quit;
/*Countries where the employee number 5 has made sales*/
proc sql;
select sum(total) FORMAT=DOLLAR13.2 as Sales_per_employee_5, c.country
from chinook.employees as e, 
	chinook.customers as c, 
	chinook.invoices as i
where e.employeeid=c.supportrepid and
	c.customerid=i.customerid and
	e.employeeid=5
group by employeeid, c.country
order by 1 desc; 
quit;
/*Countries where the employee number 3 has made sales but employee 5 did not*/
proc sql;
select distinct c.country as Countries_served_by_3_not_5
from chinook.employees as e, 
	chinook.customers as c, 
	chinook.invoices as i
where e.employeeid=c.supportrepid and
	c.customerid=i.customerid and
	e.employeeid=3
EXCEPT
select distinct c.country
from chinook.employees as e, 
	chinook.customers as c, 
	chinook.invoices as i
where e.employeeid=c.supportrepid and
	c.customerid=i.customerid and
	e.employeeid=5 ;
quit;