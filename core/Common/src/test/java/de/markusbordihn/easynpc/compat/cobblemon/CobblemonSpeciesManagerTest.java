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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import net.minecraft.resources.ResourceLocation;
import org.junit.jupiter.api.DisplayName;
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
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu.female");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu"),
        CobblemonSpeciesManager.getBaseSpeciesId(modelKey));
  }

  @Test
  void testGetBaseSpeciesIdWithGenderAndShinyVariant() {
    ResourceLocation modelKey =
        ResourceLocation.fromNamespaceAndPath("cobblemon", "charizard.female.shiny");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "charizard"),
        CobblemonSpeciesManager.getBaseSpeciesId(modelKey));
  }

  @Test
  void testGetBaseSpeciesIdWithUnderscoreInSpeciesName() {
    ResourceLocation modelKey =
        ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime.female.shiny");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime"),
        CobblemonSpeciesManager.getBaseSpeciesId(modelKey));

    ResourceLocation baseKey = ResourceLocation.fromNamespaceAndPath("cobblemon", "tapu_koko");
    assertEquals(baseKey, CobblemonSpeciesManager.getBaseSpeciesId(baseKey));
  }

  @Test
  @DisplayName("An aspect keeps its underscores instead of being split into several aspects")
  void testAspectWithUnderscore() {
    ResourceLocation modelKey =
        ResourceLocation.fromNamespaceAndPath("cobblemon", "wooloo.color-light_blue.shiny");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "wooloo"),
        CobblemonSpeciesManager.getBaseSpeciesId(modelKey));
    assertEquals(
        Set.of("color-light_blue", CobblemonSpeciesManager.VARIANT_SHINY),
        CobblemonSpeciesManager.getVariantAspects(modelKey));
  }

  @Test
  @DisplayName("Models stored before the separator changed are still resolved")
  void testLegacyVariantKeysStillResolve() {
    ResourceLocation aipom = ResourceLocation.fromNamespaceAndPath("cobblemon", "aipom");
    assertEquals(
        aipom,
        CobblemonSpeciesManager.getBaseSpeciesId(
            ResourceLocation.fromNamespaceAndPath("cobblemon", "aipom_female_shiny")));
    assertEquals(
        Set.of(CobblemonSpeciesManager.VARIANT_FEMALE, CobblemonSpeciesManager.VARIANT_SHINY),
        CobblemonSpeciesManager.getVariantAspects(
            ResourceLocation.fromNamespaceAndPath("cobblemon", "aipom_female_shiny")));
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime"),
        CobblemonSpeciesManager.getBaseSpeciesId(
            ResourceLocation.fromNamespaceAndPath("cobblemon", "mr_mime_shiny")));
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
            ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu.female")));
    assertEquals(
        Set.of(CobblemonSpeciesManager.VARIANT_FEMALE, CobblemonSpeciesManager.VARIANT_SHINY),
        CobblemonSpeciesManager.getVariantAspects(
            ResourceLocation.fromNamespaceAndPath("cobblemon", "charizard.female.shiny")));
  }

  @Test
  void testCreateVariantKey() {
    ResourceLocation speciesId = ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu");
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu.female"),
        CobblemonSpeciesManager.createVariantKey(
            speciesId, CobblemonSpeciesManager.VARIANT_FEMALE));
    assertEquals(
        ResourceLocation.fromNamespaceAndPath("cobblemon", "pikachu.female.shiny"),
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

  @Test
  @DisplayName("A variant is resolved to its species even when it is a known model itself")
  void testVariantIsNotMistakenForSpecies() {
    ResourceLocation aipom = ResourceLocation.fromNamespaceAndPath("cobblemon", "aipom");
    CobblemonSpeciesManager.setKnownSpecies(Set.of(aipom));
    try {
      for (String variant : new String[] {"aipom.shiny", "aipom.female", "aipom.female.shiny"}) {
        ResourceLocation modelKey = ResourceLocation.fromNamespaceAndPath("cobblemon", variant);
        assertEquals(aipom, CobblemonSpeciesManager.getBaseSpeciesId(modelKey));
      }
    } finally {
      CobblemonSpeciesManager.setKnownSpecies(Set.of());
    }
  }

  @Test
  @DisplayName("Aspects which cannot be part of a model name are rejected")
  void testUnsupportedAspectsAreRejected() {
    assertTrue(CobblemonSpeciesManager.isSupportedAspect("color-white"));
    assertTrue(CobblemonSpeciesManager.isSupportedAspect(CobblemonSpeciesManager.VARIANT_SHINY));

    assertTrue(CobblemonSpeciesManager.isSupportedAspect("color-light_blue"));

    assertFalse(CobblemonSpeciesManager.isSupportedAspect("color-white shiny"));
    assertFalse(CobblemonSpeciesManager.isSupportedAspect("Color-White"));
    assertFalse(CobblemonSpeciesManager.isSupportedAspect("form/alola"));
    assertFalse(CobblemonSpeciesManager.isSupportedAspect("form.alola"));
    assertFalse(CobblemonSpeciesManager.isSupportedAspect(""));
    assertFalse(CobblemonSpeciesManager.isSupportedAspect(null));
  }

  @Test
  @DisplayName("A mixed gender ratio allows the gender to be set")
  void testIsGenderMutableMixedRatio() {
    assertTrue(CobblemonSpeciesManager.isGenderMutable(0.5F));
    assertTrue(CobblemonSpeciesManager.isGenderMutable(0.1F));
    assertTrue(CobblemonSpeciesManager.isGenderMutable(0.9F));
  }

  @Test
  @DisplayName("An all-male, all-female or genderless ratio keeps the default gender")
  void testIsGenderMutableBoundaryRatios() {
    assertFalse(CobblemonSpeciesManager.isGenderMutable(0.0F));
    assertFalse(CobblemonSpeciesManager.isGenderMutable(1.0F));
    assertFalse(CobblemonSpeciesManager.isGenderMutable(-1.0F));
  }

  @Test
  @DisplayName("A female aspect requests a female gender")
  void testShouldApplyFemaleGender() {
    assertTrue(
        CobblemonSpeciesManager.shouldApplyFemaleGender(
            Set.of(CobblemonSpeciesManager.VARIANT_FEMALE)));
    assertFalse(CobblemonSpeciesManager.shouldApplyFemaleGender(Set.of()));
    assertFalse(
        CobblemonSpeciesManager.shouldApplyFemaleGender(
            Set.of(CobblemonSpeciesManager.VARIANT_SHINY)));
  }

  @Test
  @DisplayName("A shiny aspect requests a shiny pokemon")
  void testShouldApplyShiny() {
    assertTrue(
        CobblemonSpeciesManager.shouldApplyShiny(Set.of(CobblemonSpeciesManager.VARIANT_SHINY)));
    assertFalse(CobblemonSpeciesManager.shouldApplyShiny(Set.of()));
    assertFalse(
        CobblemonSpeciesManager.shouldApplyShiny(Set.of(CobblemonSpeciesManager.VARIANT_FEMALE)));
  }
}
