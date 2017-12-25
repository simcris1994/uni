/**
airline(name, leg)
airport(code, city, country, weather?)
weather(airport, condition) (clear, cloudy, windy, thunderstorms)
aircraft(regNo, model, manufacturer, type) (type: light - clear, cloudy or heavy - all except thunderstorms)
aircraftModel(model, seat)
seat(number, class, position) (pos: aisle, window, other)
adjSeat(a, b)
passenger(fName, lName, bday)
passport(passenger, country)
leg(airportA, airportB, serviceAirline, operatorAirline, aircraft) (aircraft owned by operator)
reservation(bookingCode, passenger, originAirport, destinationAirport, airline, seatNo)
itinerary(bookingCode, reservation)
visaAgreement(countryA, countryB)

operates(airline, leg)
*/


airport(cph, copenhagen, denmark, clear).
airport(lax, los_angeles, usa, clear).
airport(jfk, new_york_city, usa, clear).

aircraft(xxy, boeing737, boeing, light).
aircraft(xyy, boeing737, boeing, light).

passport(jane, sweden).
passport(mike, usa).

leg(jfk, cph, klm, sas, xxy).
leg(jfk, lax, norwegian, sas, xyy).

reservation(abc123, jane, jfk, cph, sas, a18).
reservation(bbb123, mike, jfk, cph, sas, c20).
reservation(bbb124, mike, jfk, cph, sas, a18).
reservation(abd124, jane, jfk, lax, sas, f7).
% reservation(bbb125, mike, jfk, lax, sas, f7).

visaAgreement(sweden, denmark).
visaAgreement(usa, denmark).

% problem 3
canFlyIntoAirport(Passenger, Airport) :- passport(Passenger, Country),
	airport(Airport, _, Country, _).
canFlyIntoAirport(Passenger, Airport) :- visaAgreement(PassengerCountry, AirportCountry),
	passport(Passenger, PassengerCountry),
	airport(Airport, _, AirportCountry, _).
	
canFlyIntoCountry(Passenger, Country) :- passport(Passenger, Country).
canFlyIntoCountry(Passenger, Country) :- visaAgreement(PassengerCountry, Country),
	passport(Passenger, PassengerCountry).	

% problem 4
illegalReservation(Passenger, Code) :- reservation(Code, Passenger, _, Airport, _, _),
	not(canFlyIntoAirport(Passenger, Airport)).
	
% problem 5
doubleBooking(Booking) :- reservation(Booking, Passenger1, OriginAirport, DestinationAirport, OperatorAirline, SeatNo),
	reservation(Booking2, Passenger2, OriginAirport, DestinationAirport, OperatorAirline, SeatNo),
	leg(OriginAirport, DestinationAirport, _, OperatorAirline, _),
	Passenger1 \= Passenger2,
	Booking \= Booking2.
	
doubleBookingOnAircraft(Aircraft)	:- leg(AirportA, AirportB, _, OperatorAirline, Aircraft),
	reservation(Booking, _, AirportA, AirportB, OperatorAirline, _),
	doubleBooking(Booking).

% removes duplicate results
dba(Aircraft) :- setof(Aircraft, doubleBookingOnAircraft(Aircraft), L), member(Aircraft, L).
	
canTakeOff(Aircraft) :- not(dba(Aircraft)).

canEveryPassengerFlyIntoCountry(Aircraft) :- leg(AirportA, AirportB, _, OperatorAirline, Aircraft),
	reservation(_, Passenger, AirportA, AirportB, OperatorAirline, _),
	canFlyIntoAirport(Passenger, airportB).