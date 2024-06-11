#include <iostream>
#include <string>
#include "parsers.hpp"
#include "basics.hpp"

using namespace zatsu_parser_combinator;
using parsers::RecursiveRef;
using parsers::generate_set;
using parsers::lit;
using namespace parsers::ops;
using namespace parsers::binds;
using basics::blanks;
using basics::integer;

int main() {
	const auto p = generate_set<int>([](RecursiveRef<int> expr) -> void {
		
		const auto grouped = (
			lit(u8"(") - blanks - expr - blanks - lit(u8")") >>= get<2>
		)
		| integer<int>
		<<= id;

		const auto sign = (
			*lit(u8"-") - blanks - grouped >>= choose<0, 2>
		) <<= [](std::vector<std::u8string_view>&& sign, int x) -> int {
			return sign.size() % 2 ? -x : x;
		};

		const auto mul_op = (lit(u8"*") | lit(u8"/") <<= id) - blanks - sign >>= choose<0, 2>;

		const auto mul =
			sign - *(blanks - mul_op >>= get<1>)
		<<= [](int first, std::vector<std::tuple<std::u8string_view, int>>&& rest) -> int {
			int ret = first;
			for(const auto& [op, x] : rest) {
				if(op == u8"*") {
					ret *= x;
				}
				else {
					ret /= x;
				}
			}

			return ret;
		};

		const auto add_op = (lit(u8"+") | lit(u8"-") <<= id) - blanks - mul >>= choose<0, 2>;

		expr %= mul - *(blanks - add_op >>= get<1>)
		<<= [](int first, std::vector<std::tuple<std::u8string_view, int>>&& rest) -> int {
			int ret = first;
			for(const auto& [op, x] : rest) {
				if(op == u8"+") {
					ret += x;
				}
				else {
					ret -= x;
				}
			}

			return ret;
		};
	});

	constexpr auto read_line = []() -> std::string {
		std::string ret;
		std::getline(std::cin, ret);
		return ret;
	};

	const auto input = read_line();
	const auto src = std::u8string_view{reinterpret_cast<const char8_t*>(input.data()), input.size()};
	const auto [v, r] = p->parse(src);
	std::cout << (v ? std::to_string(*v) : "failed") << std::endl;
}

/*
test case:
1 + 2 * (3 - 4) : -1
(2 + 3) * 4 - 5 : 15
-3 + (4 - 2) * 5 : 7
(6 + 2) * (3 - 5) / 3 : -5
7 - (8 / (4 + 4)) * 2 : 5
(3 + 5) * (2 - 8 / 2) : -16
(9 - 3) / (3 + 3) * 2 : 2
10 + (5 * 3 - 15) : 10
-2 * (3 + 4) + 5 : -9
(4 - 6) / (2 + 1) * 3 : 0

AC!!!
*/