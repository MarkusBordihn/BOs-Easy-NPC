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
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.SharedConstants;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.animal.CatVariant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class CatSkinVariantTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  void testOcelotMapsToTabby() {
    assertEquals(CatVariant.TABBY, CatSkinVariant.OCELOT.getVanillaVariant());
  }

  @Test
  void testDirectMappings() {
    assertEquals(CatVariant.ALL_BLACK, CatSkinVariant.ALL_BLACK.getVanillaVariant());
    assertEquals(CatVariant.BLACK, CatSkinVariant.BLACK.getVanillaVariant());
    assertEquals(CatVariant.TABBY, CatSkinVariant.TABBY.getVanillaVariant());
    assertEquals(CatVariant.WHITE, CatSkinVariant.WHITE.getVanillaVariant());
  }

  @Test
  void testAllVariantsHaveVanillaKey() {
    for (CatSkinVariant variant : CatSkinVariant.values()) {
      assertNotNull(variant.getVanillaVariant(), variant.name());
    }
  }

  @Test
  void testAllVanillaVariantsExistInRegistry() {
    for (CatSkinVariant variant : CatSkinVariant.values()) {
      assertTrue(
          BuiltInRegistries.CAT_VARIANT.containsKey(variant.getVanillaVariant().location()),
          "No vanilla cat variant registered for " + variant);
    }
  }
}
