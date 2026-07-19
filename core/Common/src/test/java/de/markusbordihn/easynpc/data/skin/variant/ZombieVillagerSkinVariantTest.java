/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.skin.variant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import de.markusbordihn.easynpc.data.profession.Profession;
import org.junit.jupiter.api.Test;

class ZombieVillagerSkinVariantTest {

  @Test
  void testAllConstantsParseBiomeAndProfession() {
    for (ZombieVillagerSkinVariant variant : ZombieVillagerSkinVariant.values()) {
      assertNotNull(variant.getVillagerBiome(), "biome for " + variant);
      assertNotNull(variant.getProfession(), "profession for " + variant);
    }
  }

  @Test
  void testNameMatchesBiomeAndProfession() {
    for (ZombieVillagerSkinVariant variant : ZombieVillagerSkinVariant.values()) {
      assertEquals(
          variant.getVillagerBiome().name() + "_" + variant.getProfession().name(),
          variant.name(),
          "name/biome/profession mismatch for " + variant);
    }
  }

  @Test
  void testSpecificMappings() {
    assertEquals(VillagerBiome.DESERT, ZombieVillagerSkinVariant.DESERT_ARMORER.getVillagerBiome());
    assertEquals(Profession.ARMORER, ZombieVillagerSkinVariant.DESERT_ARMORER.getProfession());
    assertEquals(Profession.NONE, ZombieVillagerSkinVariant.PLAINS_NONE.getProfession());
  }

  @Test
  void testDefaultIsPlainsNone() {
    assertEquals(ZombieVillagerSkinVariant.PLAINS_NONE, ZombieVillagerSkinVariant.DEFAULT);
  }
}
