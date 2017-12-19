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

passport(jane, sweden).
passport(mike, usa).

leg(jfk, cph, klm, sas, boeing).
leg(jfk, lax, norwegian, sas, boeing).

reservation(abc123, jane, jfk, cph, sas, a18).
reservation(abd124, jane, jfk, lax, sas, f7).
reservation(bbb123, mike, jfk, cph, sas, c20).
reservation(bbb124, mike, jfk, cph, sas, a18).

visaAgreement(sweden, denmark).
visaAgreement(usa, denmark).


canFlyInto(Passenger, Airport) :- passport(Passenger, Country),
	airport(Airport, _, Country, _).
canFlyInto(Passenger, Airport) :- visaAgreement(PassengerCountry, AirportCountry),
	passport(Passenger, PassengerCountry),
	airport(Airport, _, AirportCountry, _).

illegalReservation(Passenger, Code) :- reservation(Code, Passenger, _, Airport, _, _),
	not(canFlyInto(Passenger, Airport)).
	
doubleBooking(Booking) :- reservation(Booking, Passenger1, OriginAirport, DestinationAirport, OperatorAirline, SeatNo),
	reservation(Booking2, Passenger2, OriginAirport, DestinationAirport, OperatorAirline, SeatNo),
	leg(OriginAirport, DestinationAirport, _, OperatorAirline, _),
	Passenger1 \= Passenger2,
	Booking \= Booking2.
	