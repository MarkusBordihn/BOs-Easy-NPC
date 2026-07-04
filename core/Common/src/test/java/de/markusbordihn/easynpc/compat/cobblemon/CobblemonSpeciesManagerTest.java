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

package de.markusbordihn.easynpc.compat.cobblemon;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import net.minecraft.resources.ResourceLocation;
import org.junit.jupiter.api.Test;

class CobblemonSpeciesManagerTest {

  @Test
  void testGetBaseSpeciesIdWithoutVariant() {
    ResourceLocation modelKey = ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu");
    assertEquals(modelKey, CobblemonSpeciesManager.getBaseSpeciesId(modelKey));
  }

  @Test
  void testGetBaseSpeciesIdWithGenderVariant() {
    ResourceLocation modelKey =
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu_female");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu"),
        CobblemonSpeciesManager.getBaseSpeciesId(modelKey));
  }

  @Test
  void testGetBaseSpeciesIdWithGenderAndShinyVariant() {
    ResourceLocation modelKey =
        ResourceLocation.fromNamespaceAndPath("cobblemon", "charizard_female_shiny");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "charizard"),
        CobblemonSpeciesManager.getBaseSpeciesId(modelKey));
  }

  @Test
  void testGetBaseSpeciesIdWithUnderscoreInSpeciesName() {
    ResourceLocation modelKey =
        ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime_female_shiny");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime"),
        CobblemonSpeciesManager.getBaseSpeciesId(modelKey));

    ResourceLocation baseKey = ResourceLocation.fromNamespaceAndPath("cobblemon", "tapu_koko");
    assertEquals(baseKey, CobblemonSpeciesManager.getBaseSpeciesId(baseKey));
  }

  @Test
  void testGetVariantAspects() {
    assertTrue(
        CobblemonSpeciesManager.getVariantAspects(
                ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu"))
            .isEmpty());
    assertEquals(
        Set.of(CobblemonSpeciesManager.VARIANT_FEMALE),
        CobblemonSpeciesManager.getVariantAspects(
            ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu_female")));
    assertEquals(
        Set.of(CobblemonSpeciesManager.VARIANT_FEMALE, CobblemonSpeciesManager.VARIANT_SHINY),
        CobblemonSpeciesManager.getVariantAspects(
            ResourceLocation.fromNamespaceAndPath("cobblemon", "charizard_female_shiny")));
  }

  @Test
  void testCreateVariantKey() {
    ResourceLocation speciesId = ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu_female"),
        CobblemonSpeciesManager.createVariantKey(
            speciesId, CobblemonSpeciesManager.VARIANT_FEMALE));
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu_female_shiny"),
        CobblemonSpeciesManager.createVariantKey(
            speciesId,
            CobblemonSpeciesManager.VARIANT_FEMALE,
            CobblemonSpeciesManager.VARIANT_SHINY));
  }

  @Test
  void testCreateAndParseRoundTrip() {
    ResourceLocation variantKey =
        CobblemonSpeciesManager.createVariantKey(
            ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime"),
            CobblemonSpeciesManager.VARIANT_SHINY);
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime"),
        CobblemonSpeciesManager.getBaseSpeciesId(variantKey));
    assertEquals(
        Set.of(CobblemonSpeciesManager.VARIANT_SHINY),
        CobblemonSpeciesManager.getVariantAspects(variantKey));
  }
}
