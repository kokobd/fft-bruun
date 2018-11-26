#include <MatlabEngine.hpp>
#include <MatlabDataArray.hpp>
#include <vector>
#include <cstdlib>
#include <ccomplex>
#include <complex>

using namespace matlab::engine;
using namespace matlab::data;

extern "C" bool isumi_matlab_fft(
        const std::complex<double> *input,
        size_t size,
        std::complex<double> *output) {
    std::unique_ptr<MATLABEngine> engine = matlab::engine::connectMATLAB();
    if (engine == nullptr)
        return false;   
    ArrayFactory factory;
    std::vector<Array> args({
        factory.createArray<std::complex<double>>({size}, input, input + size)
    });
    const TypedArray<std::complex<double>> result = engine->feval(u"fft", args);
    for (size_t i = 0; i != size; ++i) {
        output[i] = result[i];
    }
    return true;
}
