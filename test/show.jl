function capture_show(x)
    b = IOBuffer()
    show(b, x)
    seekstart(b)
    readstring(b)
end

@testset "show" begin
    @polyvar x y z
    @test capture_show(x) == "x"
    @test capture_show(x^0) == "1"
    @test capture_show(x^1) == "x"
    @test capture_show(x^2) == "x^2"
    @test capture_show(1x^2) == "x^2"
    @test capture_show(5x) == "5x"
    @test capture_show(x * y) == "xy"
    @test capture_show(y * x) == "xy"
    @test capture_show(x + y + 5) == "x + y + 5"
    @test capture_show(y + 5 + x) == "x + y + 5"
    @test capture_show(x + x^2) == "x^2 + x"
    @test capture_show(x^2 + x) == "x^2 + x"
end
