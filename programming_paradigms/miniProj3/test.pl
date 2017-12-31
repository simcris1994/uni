% Student: Cristina Ioana Simionescu
% Email: csimio17@student.aau.dk
% Prolog engine: SWI-Prolog


% facts
% passenger(Name, Birthdate)
passenger('Jane Doe', '01-01-1990').
passenger('Jack Doe', '06-08-1988').

% passport(PassengerName, Country)
passport('Jane Doe', 'USA').
passport('Jack Doe', 'USA').

% airport(Code, City, Country, WeatherType)
airport('LAX', 'Los Angeles', 'USA', 'clear').
airport('JFK', 'New York City', 'USA', 'clear').
airport('BLL', 'Billund', 'Denmark', 'clear').
airport('CPH', 'Copenhagen', 'Denmark', 'clear').
airport('AAL', 'Aalborg', 'Denmark', 'clear').
airport('AAR', 'Aarhus', 'Denmark', 'clear').
airport('AMS', 'Amsterdam', 'The Netherlands', 'windy').

% visaAgreement(Country1, Country2)
visaAgreement('USA', 'Denmark').

% reservation(ReservationCode, PassengerName, FromAirport, ToAirport, Airline, SeatNo)
reservation('XYZ123', 'Jane Doe', 'LAX', 'BLL', 'SAS', '1A').
reservation('XXY122', 'Jack Doe', 'JFK', 'AMS', 'KLM', '1F').
reservation('ABC123', 'Jack Doe', 'LAX', 'BLL', 'SAS', '1A').
reservation('XYZ124', 'Jane Doe', 'LAX', 'CPH', 'SAS', '1E').
reservation('XYZ125', 'Jack Doe', 'BLL', 'AAL', 'SAS', '1A').
reservation('XYZ126', 'Jack Doe', 'BLL', 'AAL', 'SAS', '1B').
reservation('XYZ127', 'Jack Doe', 'BLL', 'AAL', 'SAS', '1C').
reservation('XYZ128', 'Jack Doe', 'BLL', 'AAL', 'SAS', '1D').
reservation('XYZ129', 'Jack Doe', 'BLL', 'AAL', 'SAS', '1E').
reservation('XYZ120', 'Jack Doe', 'BLL', 'AAL', 'SAS', '1F').
reservation('TTT123', 'Jack Doe', 'AAL', 'AAR', 'Norwegian', '1A').
reservation('TTT124', 'Jane Doe', 'AAL', 'AAR', 'Norwegian', '1F').

% leg(FromAirport, ToAirport, ServiceAirline, OperatorAirline, AircraftCode)
leg('LAX', 'BLL', 'SAS', 'SAS', 'BX987').
leg('BLL', 'AAL', 'SAS', 'SAS', 'BX986').
leg('LAX', 'CPH', 'SAS', 'SAS', 'BX988').
leg('JFK', 'AMS', 'KLM', 'KLM', 'AX678').
leg('AAL', 'AAR', 'Norwegian', 'Norwegian', 'AX679').

% aircraft(AircraftCode, Model, Manufacturer, Type)
aircraft('BX986', 'Airbus A380', 'Airbus', 'light').
aircraft('BX987', 'Airbus A380', 'Airbus', 'light').
aircraft('BX988', 'Airbus A380', 'Airbus', 'light').
aircraft('AX678', 'Boeing 737', 'Boeing', 'light').
aircraft('AX679', 'Boeing 737', 'Boeing', 'light').

% aircraftWeather(AicraftType, SuitableWeater)
aircraftWeather('light', 'clear').
aircraftWeather('light', 'cloudy').
aircraftWeather('heavy', 'clear').
aircraftWeather('heavy', 'cloudy').
aircraftWeather('heavy', 'windy').

% seat(AircraftCode, SeatNo, Class, SeatPosition)
seat('BX986', '1A', 'economy', 'window').
seat('BX986', '1B', 'economy', 'middle').
seat('BX986', '1C', 'economy', 'aisle').
seat('BX986', '1D', 'economy', 'aisle').
seat('BX986', '1E', 'economy', 'midlle').
seat('BX986', '1F', 'economy', 'window').
seat('BX987', '1A', 'economy', 'window').
seat('BX987', '1B', 'economy', 'middle').
seat('BX987', '1C', 'economy', 'aisle').
seat('BX987', '1D', 'economy', 'aisle').
seat('BX987', '1E', 'economy', 'midlle').
seat('BX987', '1F', 'economy', 'window').
seat('BX988', '1A', 'economy', 'window').
seat('BX988', '1B', 'economy', 'middle').
seat('BX988', '1C', 'economy', 'aisle').
seat('BX988', '1D', 'economy', 'aisle').
seat('BX988', '1E', 'economy', 'midlle').
seat('BX988', '1F', 'economy', 'window').
seat('AX678', '1A', 'economy', 'window').
seat('AX678', '1B', 'economy', 'middle').
seat('AX678', '1C', 'economy', 'aisle').
seat('AX678', '1D', 'economy', 'aisle').
seat('AX678', '1E', 'economy', 'midlle').
seat('AX678', '1F', 'economy', 'window').
seat('AX679', '1A', 'economy', 'window').
seat('AX679', '1B', 'economy', 'middle').
seat('AX679', '1C', 'economy', 'aisle').
seat('AX679', '1D', 'economy', 'aisle').
seat('AX679', '1E', 'economy', 'midlle').
seat('AX679', '1F', 'economy', 'window').
%

% an agreement between two countries goes both ways, so the order in which the countries
% are listed does not matter
agreement(Country1, Country2) :- visaAgreement(Country1, Country2);
	visaAgreement(Country2, Country1).
	
% problem 3
% a passenger may fly into an airport if either that airport is in the country that
% issued their passport or in a country that has a visa agreement with the country that
% issued the passport
mayFlyInto(Passenger, Airport) :- passport(Passenger, Country),
	airport(Airport, _, Country, _).
mayFlyInto(Passenger, Airport) :- passport(Passenger, PassengerCountry),
	airport(Airport, _, AirportCountry, _),
	agreement(PassengerCountry, AirportCountry).	
%
	
% problem 4
% a reservation for a flight from airport A to B is legal only if the passenger is 
% allowed to fly into aiport B
isLegal(Reservation, Passenger) :- reservation(Reservation, Passenger, _, Destination, _, _),
	mayFlyInto(Passenger, Destination).
%

% problem 5
% a booking is double if there exist two different reservations made by two different 
% passengers, on the same leg (i.e. origin and destination airports match) and with the
% same seat
isDouble(Reservation1) :- reservation(Reservation1, Passenger1, Origin, Destination, _, SeatNo),
	reservation(Reservation2, Passenger2, Origin, Destination, _, SeatNo),
	Passenger1 \= Passenger2,
	Reservation1 \= Reservation2.
%

% problem 6
% returns the number of double bookings on an aircraft by making a list of all double 
% bookings on that aircraft and getting its length
countDoubleBookingsOnAircraft(Aircraft, No) :-
    findall(X, (reservation(X, _, Origin, Destination, _, _), leg(Origin, Destination, _, _, Aircraft), isDouble(X)), Bookings),
    length(Bookings, No).

% returns the number of passengers who cannot travel on a specific aircraft by making a 
% list of all the passengers who have made a reservation on that aircraft, but may not 
% fly into the airport that the aircraft lands on and then gets the length of the list
countPassengersWhoCantTravelOnAircraft(Aircraft, No) :-
	findall(X, (passenger(X, _), reservation(_, X, Origin, Destination, _, _), leg(Origin, Destination, _, _, Aircraft), not(mayFlyInto(X, Destination))), Passengers),
	length(Passengers, No).
	
% rule for whether the weather at the origin and destination airport are suitable for that
% particular aircraft
isWeatherSuitableForAircraft(Aircraft) :- aircraft(Aircraft, _, _, Type),
	leg(Origin, Destination, _, _, Aircraft),
	aircraftWeather(Type, Weather),
	airport(Origin, _, _, Weather),
	airport(Destination, _, _, Weather).
	
% an aircraft may fly from aiport A to B if there are no double bookings on it, if all 
% the passengers that have made reservations are allowed to enter airport B (meaning 
% that country), and if the weather at both aiports is suitable for the aircraft
mayTakeOff(Aircraft) :- aircraft(Aircraft, _, _, _),
	countDoubleBookingsOnAircraft(Aircraft, 0),
	countPassengersWhoCantTravelOnAircraft(Aircraft, 0),
	isWeatherSuitableForAircraft(Aircraft).
%

% problem 7

% two airports A and B are connected if either there is a leg between A and B or a set 
% of intermediate legs that start from A and end in B
connected(A, B) :- leg(A, B, _, _, _),
	airport(A, _, _, _),
	airport(B, _, _, _).
connected(A, B) :- leg(A, X, _, _, _),
	connected(X, B),
	airport(A, _, _, _),
	airport(B, _, _, _),
	airport(X, _, _, _).
	
% rule similar to connected(A, B), but the airline must be the same for all legs that
% constitute the route
connectedSameAirline(A, B, Airline) :- leg(A, B, Airline, _, _),
	airport(A, _, _, _),
	airport(B, _, _, _).
connectedSameAirline(A, B, Airline) :- leg(A, X, Airline, _, _),
	connectedSameAirline(X, B, Airline),
	airport(A, _, _, _),
	airport(B, _, _, _),
	airport(X, _, _, _).
	
% rule similar to connected(A, B), but the aircraft manufacturer must be the same for 
% all legs that are on that route
connectedSameAircraft(A, B, Aircraft) :- leg(A, B, _, _, AircraftNo),
	aircraft(AircraftNo, _, Aircraft, _),
	airport(A, _, _, _),
	airport(B, _, _, _).
connectedSameAircraft(A, B, Aircraft) :- leg(A, X, _, _, AircraftNo),
	aircraft(AircraftNo, _, Aircraft, _),
	connectedSameAircraft(X, B, Aircraft),
	airport(A, _, _, _),
	airport(B, _, _, _),
	airport(X, _, _, _).	
	
% given two airports, this will return a list of all airports that are on that route,
% provided the airports are connected
path(A, B, [A, B]) :-
    leg(A, B, _, _, _),
    airport(A, _, _, _),
		airport(B, _, _, _).
path(A, B, PathAB) :-
    leg(A, C, _, _, _),
    path(C, B, PathCB),
    airport(A, _, _, _),
		airport(B, _, _, _),
		airport(C, _, _, _),
    PathAB = [A | PathCB].
  
% given two aiports, this will return a list of all aircrafts that operate the legs that
% are part of that route, provided there is one
aircraftsBetween(A, B, [Aircraft]) :-
    leg(A, B, _, _, Aircraft),
    airport(A, _, _, _),
		airport(B, _, _, _).
aircraftsBetween(A, B, AircraftsAB) :-
    leg(A, C, _, _, Aircraft),
    aircraftsBetween(C, B, AircraftsCB),
    airport(A, _, _, _),
		airport(B, _, _, _),
		airport(C, _, _, _),
    AircraftsAB = [Aircraft | AircraftsCB].
 
% returns a list of airports that a pasenger is not allowed to fly into
airportsPassengerMayNotVisit(Passenger, Airports) :- findall(X, (airport(X, _, _, _), not(mayFlyInto(Passenger, X))), Airports).
% returns a list of airports that a passenger is not allowed to fly into that are on
% the path from airport A to B, by calculating the intersection between the previous list
% and the list of all airports that are on the path from A to B
airportsOnRoutePassengerMayNotVisit(Passenger, A, B, AirportList) :- 
	path(A, B, Path),
	airportsPassengerMayNotVisit(Passenger, Airports),
	intersection(Path, Airports, AirportList).
% gets the count of airports that a passenger may not fly into by getting the length of
% the list above
countAirportsCantVisit(Passenger, A, B, No) :-
	airportsOnRoutePassengerMayNotVisit(Passenger, A, B, AirportList),
	length(AirportList, No).

% a seat is reserved on an aircraft if a booking has been made for the leg that the aircraft
% operates
reservedSeatOnAircraft(Aircraft, Seat) :- aircraft(Aircraft, _, _, _),
	leg(Origin, Destination, _, _, Aircraft),
	reservation(_, _, Origin, Destination, _, Seat).

% makes a list of all seats on an aircraft that are not reserved
freeSeatsOnAircraft(Aircraft, Seats) :- findall(X, (seat(Aircraft, X, _, _), (not(reservedSeatOnAircraft(Aircraft, X)))), Seats).

% given a list of aircrafts, it check if all of them have free seats
aircraftsWithFreeSeats([Aircraft]) :- 
	freeSeatsOnAircraft(Aircraft, Seats),
	length(Seats, NoSeats),
	NoSeats > 0.
aircraftsWithFreeSeats([X | Aircrafts]) :- 
	freeSeatsOnAircraft(X, Seats),
	length(Seats, NoSeats),
	NoSeats > 0,
	aircraftsWithFreeSeats(Aircrafts).
	
% there are available seats between two airports if all the aircrafts that operate the 
% legs that are on the route have free seats
seatsAvailableBetween(A, B) :- aircraftsBetween(A, B, Aircrafts),
	aircraftsWithFreeSeats(Aircrafts).
	
% similar to freeSeatsOnAircraft, but the type of seat must match
freeSeatsOnAircraftWithType(Aircraft, Type, Seats) :- findall(X, (seat(Aircraft, X, _, Type), (not(reservedSeatOnAircraft(Aircraft, X)))), Seats).
aircraftsWithFreeSeatsWithType([Aircraft], Type) :- 
	freeSeatsOnAircraftWithType(Aircraft, Type, Seats),
	length(Seats, NoSeats),
	NoSeats > 0.
aircraftsWithFreeSeatsWithType([X | Aircrafts], Type) :- 
	freeSeatsOnAircraftWithType(X, Type, Seats),
	length(Seats, NoSeats),
	NoSeats > 0,
	aircraftsWithFreeSeatsWithType(Aircrafts, Type).

% similar to seatsAvailableBetween, but the type of seat is specified
seatsAvailableBetweenWithType(A, B, Type) :- aircraftsBetween(A, B, Aircrafts),
	aircraftsWithFreeSeatsWithType(Aircrafts, Type).	
	
% a passenger can make a booking from airport A to B if the airports are connected by one
% or more legs, if the passenger is allowed to fly into all the airports that are on the
% route and if there are seats available on all the legs
canPassengerBook(Passenger, A, B) :- passenger(Passenger, _),
	connected(A, B),
	countAirportsCantVisit(Passenger, A, B, 0),
	seatsAvailableBetween(A, B).
	
% similar to canPassengerBook, but the legs connecting aiport A and B must be serviced by
% same airline
canPassengerBookSameAirline(Passenger, A, B) :- passenger(Passenger, _),
	connectedSameAirline(A, B, _),
	countAirportsCantVisit(Passenger, A, B, 0),
	seatsAvailableBetween(A, B).
	
% similar to canPassengerBook, but all the aircrafts that operate the legs on the route
% from airport A to B must be manufactured by Boeing
canPassengerBookSameAircraft(Passenger, A, B) :- passenger(Passenger, _),
	connectedSameAircraft(A, B, 'Boeing'),
	countAirportsCantVisit(Passenger, A, B, 0),
	seatsAvailableBetween(A, B).

% similar to canPassengerBook, but all the aircrafts that operate the legs from aiport 
A to B must have window seats available
canPassengerBookOnlyWindow(Passenger, A, B) :- passenger(Passenger, _),
	connected(A, B),
	countAirportsCantVisit(Passenger, A, B, 0),
	seatsAvailableBetweenWithType(A, B, 'window').	
	