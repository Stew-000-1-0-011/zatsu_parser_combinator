#pragma once

#include <concepts>

#include "parsers.hpp"

namespace zatsu_parser_combinator::basics::impl {
	using parsers::lit;
	using parsers::RecursiveRef;
	using parsers::generate_set;
	using namespace parsers::ops;
	using namespace parsers::binds;

	struct None final {};

	inline constexpr auto blank = lit(u8" ") | lit(u8"\t") | lit(u8"\n") | lit(u8"\r") <<= id;
	inline constexpr auto blanks = *blank;

	inline constexpr auto nonzero = (
		lit(u8"1") | lit(u8"2") | lit(u8"3") | lit(u8"4") | lit(u8"5") | lit(u8"6") | lit(u8"7") | lit(u8"8") | lit(u8"9")
		<<= id
	)
	>>= [](std::u8string_view x) -> char {
		return x[0] - u8'0';
	};

	inline constexpr auto digit = (
		lit(u8"0") >>= [](std::u8string_view) -> char {
			return 0;
		}
	) | nonzero
	<<= id;

	template<std::integral T_>
	inline constexpr auto integer = nonzero - *digit <<= [](char head, std::vector<char>&& tails) -> T_ {
		T_ ret = head;
		for(const T_ d : tails) {
			ret = ret * 10 + d;
		}

		return ret;
	};
}

namespace zatsu_parser_combinator::basics {
	using impl::blank;
	using impl::blanks;
	using impl::nonzero;
	using impl::digit;
	using impl::integer;
}