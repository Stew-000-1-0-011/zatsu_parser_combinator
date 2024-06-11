#pragma once

namespace zatsu_parser_combinator::utility {
	template<class T_, class U_>
	[[nodiscard]] constexpr auto&& forward_like(U_&& x) noexcept
	{
		constexpr bool is_adding_const = std::is_const_v<std::remove_reference_t<T_>>;
		if constexpr (std::is_lvalue_reference_v<T_&&>)
		{
			if constexpr (is_adding_const)
				return std::as_const(x);
			else
				return static_cast<U_&>(x);
		}
		else
		{
			if constexpr (is_adding_const)
				return std::move(std::as_const(x));
			else
				return std::move(x);
		}
	}
}