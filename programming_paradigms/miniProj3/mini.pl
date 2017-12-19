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

countryOfAirport(cph, denmark).
countryOfAirport(lax, usa).
passport(jane, sweden).
passport(mike, usa).
reservedBy(abc123, jane).
reservedBy(abd124, jane).
reservedBy(bbb123, mike).
reservationDestination(abc123, cph).
reservationDestination(abd124, lax).
reservationDestination(bbb123, cph).
visaAgreement(sweden, denmark).


canFlyInto(Passenger, Airport) :- passport(Passenger, Country),
	countryOfAirport(Airport, Country).
canFlyInto(Passenger, Airport) :- visaAgreement(PassengerCountry, AirportCountry),
	passport(Passenger, PassengerCountry),
	countryOfAirport(Airport, AirportCountry).

illegalReservation(Passenger, Reservation) :- reservedBy(Reservation, Passenger),
	reservationDestination(Reservation, Airport),
	not(canFlyInto(Passenger, Airport)).