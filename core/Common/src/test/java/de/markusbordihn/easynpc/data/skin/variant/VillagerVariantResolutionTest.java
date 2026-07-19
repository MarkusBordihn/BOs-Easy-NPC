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
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.profession.Profession;
import net.minecraft.SharedConstants;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class VillagerVariantResolutionTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  void testEveryProfessionResolvesToVanillaRegistryEntry() {
    for (Profession profession : Profession.values()) {
      assertTrue(
          BuiltInRegistries.VILLAGER_PROFESSION.containsKey(profession.getRegistryKey()),
          "No vanilla villager profession registered for "
              + profession
              + " ("
              + profession.getRegistryKey()
              + ")");
    }
  }

  @Test
  void testEveryBiomeResolvesToVanillaVillagerType() {
    for (VillagerBiome biome : VillagerBiome.values()) {
      assertTrue(
          BuiltInRegistries.VILLAGER_TYPE.containsKey(biome.getRegistryKey()),
          "No vanilla villager type registered for " + biome + " (" + biome.getRegistryKey() + ")");
    }
  }

  @Test
  void testNoneProfessionResolvesToVanillaNone() {
    // The refactor depends on NONE mapping to the real minecraft:none profession (not null),
    // so a *_NONE villager variant keeps a valid, professionless villager instead of a stale one.
    assertEquals(new ResourceLocation("none"), Profession.NONE.getRegistryKey());
    assertTrue(
        BuiltInRegistries.VILLAGER_PROFESSION.containsKey(Profession.NONE.getRegistryKey()),
        "Vanilla registry is missing the expected minecraft:none villager profession");
  }
}
