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

import net.minecraft.SharedConstants;
import net.minecraft.core.registries.Registries;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.animal.feline.CatVariants;
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
    assertEquals(CatVariants.TABBY, CatSkinVariant.OCELOT.getVanillaVariant());
  }

  @Test
  void testDirectMappings() {
    assertEquals(CatVariants.ALL_BLACK, CatSkinVariant.ALL_BLACK.getVanillaVariant());
    assertEquals(CatVariants.BLACK, CatSkinVariant.BLACK.getVanillaVariant());
    assertEquals(CatVariants.TABBY, CatSkinVariant.TABBY.getVanillaVariant());
    assertEquals(CatVariants.WHITE, CatSkinVariant.WHITE.getVanillaVariant());
  }

  @Test
  void testAllVariantsHaveVanillaKey() {
    for (CatSkinVariant variant : CatSkinVariant.values()) {
      assertNotNull(variant.getVanillaVariant(), variant.name());
    }
  }

  @Test
  void testAllVanillaVariantsBelongToCatVariantRegistry() {
    for (CatSkinVariant variant : CatSkinVariant.values()) {
      assertEquals(
          Registries.CAT_VARIANT,
          variant.getVanillaVariant().registryKey(),
          "Unexpected registry for " + variant);
    }
  }
}
