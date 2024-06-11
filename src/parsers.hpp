#pragma once

#include <tuple>
#include <utility>
#include <type_traits>
#include <concepts>
#include <string_view>
#include <optional>
#include <variant>
#include <vector>
#include <memory>

#include "std_types.hpp"
#include "utility.hpp"

namespace zatsu_parser_combinator::parsers::impl {

	template<class V_>
	using Output = std::pair<std::optional<V_>, std::u8string_view>;

	template<class V_>
	struct ParserExprBase {
		using V = V_;

		constexpr ~ParserExprBase() = default;
		virtual constexpr auto parse(std::u8string_view src) const -> Output<V_> = 0;
	};

	template<class T_>
	concept parser_expr_ref = requires(const T_ imut) {
		{[]<class V_>(const ParserExprBase<V_>&){}(imut)};
	};

	template<class T_>
	concept parser_expr = parser_expr_ref<T_> && (!std::is_reference_v<T_>);

	struct Literal final : ParserExprBase<std::u8string_view> {
		std::u8string_view value;

		constexpr auto parse(std::u8string_view src) const -> Output<std::u8string_view> override {
			if (src.starts_with(this->value)) {
				return {this->value, src.substr(this->value.size())};
			}
			return {std::nullopt, src};
		}
	};

	template<parser_expr ... PEs_>
	struct And final : ParserExprBase<std::tuple<typename PEs_::V ...>> {
		std::tuple<PEs_ ...> ps;

		constexpr And(const std::tuple<PEs_ ...>& ps) : ps{ps} {}

		constexpr auto parse(std::u8string_view src) const -> Output<std::tuple<typename PEs_::V ...>> override {
			auto result = std::tuple<std::optional<typename PEs_::V> ...>{};
			auto rest = src;
			bool failed = false;

			const auto parse_i = [this, &result, &rest, &failed]<usize i_>() -> void {
				if(failed) {
					return;
				}

				auto [v, r] = std::get<i_>(this->ps).parse(rest);
				if(v) {
					std::get<i_>(result) = v;
					rest = std::move(r);
				}
				else {
					failed = true;
				}
			};
			
			return [this, src, &parse_i, &result, &rest, &failed]<usize ... indices_>(std::index_sequence<indices_ ...>) -> Output<std::tuple<typename PEs_::V ...>> {
				(parse_i.template operator()<indices_>(), ...);

				if(failed) {
					return {std::nullopt, src};
				}
				else {
					return {std::make_optional<std::tuple<typename PEs_::V ...>>(std::move(*std::get<indices_>(result)) ...), rest};
				}
			}(std::index_sequence_for<PEs_ ...>{});
		}
	};

	template<parser_expr ... PEs_>
	struct Or final : ParserExprBase<std::variant<typename PEs_::V ...>> {
		std::tuple<PEs_ ...> ps;

		constexpr Or(const std::tuple<PEs_ ...>& ps) : ps{ps} {}

		constexpr auto parse(std::u8string_view src) const -> Output<std::variant<typename PEs_::V ...>> override {
			auto result = std::variant<typename PEs_::V ...>{};
			auto rest = src;
			bool failed = true;

			const auto parse_i = [this, &result, &rest, &failed]<usize i_>(std::u8string_view src) -> void {
				if(failed) {
					auto [v, r] = std::get<i_>(this->ps).parse(src);
					if(v) {
						result.template emplace<i_>(std::move(*v));
						rest = r;
						failed = false;
					}
				}
			};

			[&parse_i, src]<usize ... indices_>(std::index_sequence<indices_ ...>) -> void {
				(parse_i.template operator()<indices_>(src), ...);
			}(std::index_sequence_for<PEs_ ...>{});

			if(failed) {
				return {std::nullopt, src};
			}
			else {
				return {std::make_optional(std::move(result)), rest};
			}
		}
	};

	template<parser_expr PE_>
	struct Star final : ParserExprBase<std::vector<typename PE_::V>> {
		PE_ p;

		constexpr Star(const PE_& p) : p{p} {}

		constexpr auto parse(std::u8string_view src) const -> Output<std::vector<typename PE_::V>> override {
			auto result = std::vector<typename PE_::V>{};
			auto rest = src;

			while(true) {
				auto [v, r] = this->p.parse(rest);
				if(v) {
					result.emplace_back(std::move(*v));
					rest = r;
				}
				else {
					break;
				}
			}

			return {std::make_optional(std::move(result)), rest};
		}
	};

	template<parser_expr PE_>
	struct Plus final : ParserExprBase<std::vector<typename PE_::V>> {
		PE_ p;

		constexpr Plus(const PE_& p) : p{p} {}

		constexpr auto parse(std::u8string_view src) const -> Output<std::vector<typename PE_::V>> override {
			auto result = std::vector<typename PE_::V>{};
			auto rest = src;

			auto [v0, r0] = this->p.parse(rest);
			if(v0) {
				result.emplace_back(std::move(*v0));
				rest = r0;

				while(true) {
					auto [v, r] = this->p.parse(rest);
					if(v) {
						result.emplace_back(std::move(*v));
						rest = r;
					}
					else {
						break;
					}
				}

				return {std::make_optional(std::move(result)), rest};
			}
			else {
				return {std::nullopt, src};
			}
		}
	};

	template<parser_expr PE_>
	struct Opt final : ParserExprBase<std::optional<typename PE_::V>> {
		PE_ p;

		constexpr Opt(const PE_& p) : p{p} {}

		constexpr auto parse(std::u8string_view src) const -> Output<std::optional<typename PE_::V>> override {
			auto [v, r] = this->p.parse(src);
			if(v) {
				return {std::make_optional<std::optional<typename PE_::V>>(std::move(*v)), r};
			}
			else {
				return {std::make_optional<std::optional<typename PE_::V>>(std::nullopt), src};
			}
		}
	};

	template<parser_expr PE_, class F_>
	requires (!std::is_reference_v<F_>)
	struct Bind final : ParserExprBase<std::invoke_result_t<F_, typename PE_::V>> {
		PE_ p;
		F_ f;

		template<parser_expr_ref PEref_, class Fref_>
		constexpr Bind(PEref_&& p, Fref_&& f)
			: p{std::forward<PEref_>(p)}
			, f{std::forward<Fref_>(f)}
		{
			static_assert(std::is_same_v<std::remove_cvref_t<Fref_>, F_>);
		}

		constexpr auto parse(std::u8string_view src) const -> Output<std::invoke_result_t<F_, typename PE_::V>> override {
			auto [v, r] = this->p.parse(src);
			if(v) {
				return {this->f(std::move(*v)), r};
			}
			else {
				return {std::nullopt, src};
			}
		}
	};
	template<parser_expr_ref PEref_, class Fref_>
	Bind(PEref_&&, Fref_&&) -> Bind<std::remove_cvref_t<PEref_>, std::remove_cvref_t<Fref_>>;

	template<class V_>
	struct RecursiveCore final {
		std::unique_ptr<ParserExprBase<V_>> p;
	};

	template<class V_>
	struct RecursiveRef final : ParserExprBase<V_> {
		RecursiveCore<V_> * p;

		constexpr RecursiveRef(RecursiveCore<V_> * p) : p{p} {}

		constexpr auto parse(std::u8string_view src) const -> Output<V_> override {
			return this->p->p->parse(src);
		}
	};

	template<class VHead_, class ... Vs_>
	struct ParserSet final : ParserExprBase<VHead_> {
		std::tuple<RecursiveCore<VHead_>, RecursiveCore<Vs_> ...> cores;

		constexpr auto parse(std::u8string_view src) const -> Output<VHead_> override {
			return std::get<0>(this->cores).p->parse(src);
		}
	};

	
	template<class ... Vs_>
	inline constexpr auto generate_set(auto&& f) -> std::unique_ptr<ParserSet<Vs_ ...>> {
		std::unique_ptr<ParserSet<Vs_ ...>> set = std::make_unique<ParserSet<Vs_ ...>>();
		
		[&f, &set]<usize ... indices_>(std::index_sequence<indices_ ...>) {
			f(RecursiveRef<Vs_>{&std::get<indices_>(set->cores)} ...);
		}(std::index_sequence_for<Vs_ ...>{});

		return set;
	}

	inline constexpr auto lit(std::u8string_view value) -> Literal {
		Literal ret{};
		ret.value = value;
		return ret;
	}

	namespace ops {
		namespace impl {
			template<class T_, template<class ...> class PList_>
			concept is_plist = requires(const T_ imut) {
				{[]<parser_expr ... PEs_>(const PList_<PEs_ ...>&){}(imut)};
			};

			template<template<class ...> class PList_, parser_expr_ref PEref_>
			inline constexpr auto lift_to_tup(PEref_&& t) {
				if constexpr(is_plist<std::remove_cvref_t<PEref_>, PList_>) {
					return utility::forward_like<PEref_>(t.ps);
				}
				else {
					return std::tuple<std::remove_cvref_t<PEref_>>{std::forward<PEref_>(t)};
				}
			}
		}

		template<parser_expr_ref Lref_, parser_expr_ref Rref_>
		inline constexpr auto operator-(Lref_&& l, Rref_&& r) {
			return And{std::tuple_cat(impl::lift_to_tup<And>(std::forward<Lref_>(l)), impl::lift_to_tup<And>(std::forward<Rref_>(r)))};
		}

		template<parser_expr_ref Lref_, parser_expr_ref Rref_>
		inline constexpr auto operator|(Lref_&& l, Rref_&& r) {
			return Or{std::tuple_cat(impl::lift_to_tup<Or>(std::forward<Lref_>(l)), impl::lift_to_tup<Or>(std::forward<Rref_>(r)))};
		}

		template<parser_expr_ref PEref_>
		inline constexpr auto operator*(PEref_&& p) {
			return Star{std::forward<PEref_>(p)};
		}

		template<parser_expr_ref PEref_>
		inline constexpr auto operator+(PEref_&& p) {
			return Plus{std::forward<PEref_>(p)};
		}

		template<parser_expr_ref PEref_>
		inline constexpr auto operator!(PEref_&& p) {
			return Opt{std::forward<PEref_>(p)};
		}

		template<parser_expr_ref PEref_, class Fref_>
		inline constexpr auto operator>>=(PEref_&& p, Fref_&& f) -> Bind<std::remove_cvref_t<PEref_>, std::remove_cvref_t<Fref_>> {
			using PE = std::remove_cvref_t<PEref_>;
			using F = std::remove_cvref_t<Fref_>;
			return Bind<PE, F>{std::forward<PEref_>(p), std::forward<Fref_>(f)};
		}

		template<parser_expr_ref PEref_, class Fref_>
		inline constexpr auto operator<<=(PEref_&& p, Fref_&& f) -> auto {
			using PE = std::remove_cvref_t<PEref_>;
			using F = std::remove_cvref_t<Fref_>;

			if constexpr(requires(PE::V v){{[]<class ... Vs2_>(const std::tuple<Vs2_ ...>&){}(v)};}) {
				return p >>= [f = std::forward<Fref_>(f)](auto&& x) -> auto {
					return std::apply(f, std::move(x));
				};
			}
			else if constexpr(requires(PE::V v){{[]<class ... Vs2_>(const std::variant<Vs2_ ...>&){}(v)};}) {
				return p >>= [f = std::forward<Fref_>(f)](auto&& x) -> auto {
					return std::visit(f, std::move(x));
				};
			}
			else if constexpr(requires(PE::V v){{[]<class V2_>(const std::vector<V2_>&){}(v)};}) {
				return p >>= [f = std::forward<Fref_>(f)](auto&& x) -> auto {
					std::vector<std::invoke_result_t<F, typename PE::V::value_type>> ret{};
					ret.reserve(x.size());
					for(auto&& e : x) {
						ret.emplace_back(f(std::move(e)));
					}
					return ret;
				};
			}
			else if constexpr(requires(PE::V v){{[]<class V2_>(const std::optional<V2_>&){}(v)};}) {
				return p >>= [f = std::forward<Fref_>(f)](auto&& x) -> auto {
					if(x) {
						return std::make_optional(f(std::move(*x)));
					}
					else {
						return std::nullopt;
					}
				};
			}
			else {
				static_assert([]{return false;}(), "Invalid operator<<= usage");
			}
			
		}

		template<parser_expr_ref PEref_>
		inline constexpr void operator%=(RecursiveRef<typename std::remove_cvref_t<PEref_>::V>& ref, PEref_&& p) {
			ref.p->p = std::make_unique<std::remove_cvref_t<PEref_>>(std::forward<PEref_>(p));
		}
	}

	namespace binds {
		inline constexpr auto id = [](auto&& x) -> auto {
			return x;
		};

		template<usize ... indices_>
		inline constexpr auto choose = []<class ... Vs_>(std::tuple<Vs_ ...>&& x) -> std::tuple<std::tuple_element_t<indices_, std::tuple<Vs_ ...>> ...> {
			return std::tuple<std::tuple_element_t<indices_, std::tuple<Vs_ ...>> ...>{std::get<indices_>(x) ...};
		};

		template<usize index>
		inline constexpr auto get = []<class ... Vs_>(std::tuple<Vs_ ...>&& x) -> std::tuple_element_t<index, std::tuple<Vs_ ...>> {
			return std::get<index>(x);
		};

		inline constexpr auto debug_type = [](auto&& x) -> auto {
			static_assert(std::is_same_v<decltype(x), void>, "debug_type");
		};
	}
}

namespace zatsu_parser_combinator::parsers {
	using impl::RecursiveRef;
	using impl::lit;
	using impl::generate_set;
	namespace ops = impl::ops;
	namespace binds = impl::binds;
}
