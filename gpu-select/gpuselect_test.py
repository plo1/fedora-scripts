import unittest
import gpuselect


class TestGPUSelect(unittest.TestCase):
    """Documentation for TestGPUSelect

    """
    def setUp(self):
        self.maxDiff = None
        with open('grub_nvidia', 'r') as f:
            self.grub_nvidia = f.read()
        with open('grub_intel', 'r') as f:
            self.grub_intel = f.read()

    def test_intel2intel(self):
        newcfg = gpuselect.select('intel', self.grub_intel)
        self.assertEqual(self.grub_intel, newcfg)

    def test_intel2nvidia(self):
        newcfg = gpuselect.select('nvidia', self.grub_intel)
        self.assertEqual(self.grub_nvidia, newcfg)

    def test_nvidia2nvidia(self):
        newcfg = gpuselect.select('nvidia', self.grub_nvidia)
        self.assertEqual(self.grub_nvidia, newcfg)

    def test_nvidia2intel(self):
        newcfg = gpuselect.select('intel', self.grub_nvidia)
        self.assertEqual(self.grub_intel, newcfg)


if __name__ == '__main__':
    unittest.main()
