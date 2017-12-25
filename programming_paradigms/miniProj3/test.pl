/**
airline(Name, Legs).
airport(Code, City, Country, Weather).
aircraft(RegNo, Model, Manufacturer, Seats, WeatherType).
seat(Number, Class, Position, Left, Right).
aircraftWeather(light, clear).
aircraftWeather(light, cloudy).
aircraftWeather(heavy, clear).
aircraftWeather(heavy, cloudy).
aircraftWeather(heavy, windy).
passenger(Name, BDay).
passport(Passenger, Country).
leg(FromAirport, ToAirport, ServiceAirline, OperatorAirline, Aircraft).
reservation(Code, Passenger, Origin, Destination, Airline, SeatNo).
itinerary(BookingCode, Reservations).
visaAgreement(Country1, Country2).
*/

passenger('Jane Doe', '01-01-1990').
passenger('Jack Doe', '06-08-1988').
passport('Jane Doe', 'USA').
passport('Jack Doe', 'USA').
airport('LAX', 'Los Angeles', 'USA', 'clear').
airport('JFK', 'New York City', 'USA', 'clear').
airport('BLL', 'Billund', 'Denmark', 'clear').
airport('AMS', 'Amsterdam', 'The Netherlands', 'windy').
visaAgreement('USA', 'Denmark').
reservation('XYZ123', 'Jane Doe', 'LAX', 'BLL', 'SAS', '12A').
reservation('XXY122', 'Jack Doe', 'JFK', 'AMS', 'KLM', '30F').
reservation('ABC123', 'Jack Doe', 'LAX', 'BLL', 'SAS', '12A').
leg('LAX', 'BLL', 'SAS', 'SAS', 'BX987').
leg('JFK', 'AMS', 'KLM', 'KLM', 'AX678').
aircraft('BX987', 'Airbus A380', 'Airbus', 'light').
aircraft('AX678', 'Boeing 737', 'Boeing', 'light').

agreement(Country1, Country2) :- visaAgreement(Country1, Country2);
	visaAgreement(Country2, Country1).
	
% problem 3
mayFlyInto(Passenger, Airport) :- passport(Passenger, Country),
	airport(Airport, _, Country, _).
mayFlyInto(Passenger, Airport) :- passport(Passenger, PassengerCountry),
	airport(Airport, _, AirportCountry, _),
	agreement(PassengerCountry, AirportCountry).	
%
	
% problem 4
isLegal(Reservation, Passenger) :- reservation(Reservation, Passenger, _, Destination, _, _),
	mayFlyInto(Passenger, Destination).
	
isIllegal(Reservation, Passenger) :- reservation(Reservation, Passenger, _, _, _, _),
	not(isLegal(Reservation, Passenger)).
%

% problem 5
isDouble(Reservation1) :- reservation(Reservation1, Passenger1, Origin, Destination, _, SeatNo),
	reservation(Reservation2, Passenger2, Origin, Destination, _, SeatNo),
	Passenger1 \= Passenger2,
	Reservation1 \= Reservation2.
	
notDouble(Reservation) :- reservation(Reservation, _, _, _, _, _),
	not(isDouble(Reservation)).
%	

% problem 6
bookingsOnAircraft(Aircraft, Bookings) :- findall(X, (reservation(X, _, Origin, Destination, _, _), leg(Origin, Destination, _, _, Aircraft)), Bookings).
doubleBookingsOnAircraft(Aircraft, Bookings) :- findall(X, (reservation(X, _, Origin, Destination, _, _), leg(Origin, Destination, _, _, Aircraft), isDouble(X)), Bookings).
countDoubleBookingsOnAircraft(Aircraft, No) :-
    findall(X, (reservation(X, _, Origin, Destination, _, _), leg(Origin, Destination, _, _, Aircraft), isDouble(X)), Bookings),
    length(Bookings, No).
passengersOnAircraft(Aircraft, Passengers) :- findall(X, (passenger(X, _), reservation(_, X, Origin, Destination, _, _), leg(Origin, Destination, _, _, Aircraft)), Passengers).
passengersWhoCantTravelOnAircraft(Aircraft, Passengers) :- findall(X, (passenger(X, _), reservation(_, X, Origin, Destination, _, _), leg(Origin, Destination, _, _, Aircraft), not(mayFlyInto(X, Destination))), Passengers).
/**doubleBookingOnAircraft(Aircraft) :- aircraft(Aircraft, _, _, _),
	leg(Origin, Destination, ServiceAirline, OperatorAirline, Aircraft),*/
	
	