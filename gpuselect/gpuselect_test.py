import unittest
import gpuselect


class TestGPUSelect(unittest.TestCase):
    """Documentation for TestGPUSelect

    """

    def setUp(self):
        self.maxDiff = None
        self.path_nvidia = 'grub_nvidia'
        self.path_intel = 'grub_intel'
        with open(self.path_nvidia, 'r') as f:
            self.grub_nvidia = f.read()
        with open(self.path_intel, 'r') as f:
            self.grub_intel = f.read()

    def test_intel2intel(self):
        newcfg, oldcfg = gpuselect.select('intel', self.path_intel)
        self.assertEqual(self.grub_intel, newcfg)

    def test_intel2nvidia(self):
        newcfg, oldcfg = gpuselect.select('nvidia', self.path_intel)
        self.assertEqual(self.grub_nvidia, newcfg)

    def test_nvidia2nvidia(self):
        newcfg, oldcfg = gpuselect.select('nvidia', self.path_nvidia)
        self.assertEqual(self.grub_nvidia, newcfg)

    def test_nvidia2intel(self):
        newcfg, oldcfg = gpuselect.select('intel', self.path_nvidia)
        self.assertEqual(self.grub_intel, newcfg)


if __name__ == '__main__':
    unittest.main()
