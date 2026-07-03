/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.client.compat.cobblemon;

import com.cobblemon.mod.common.client.render.ModelAssetVariation;
import com.cobblemon.mod.common.client.render.VaryingRenderableResolver;
import com.cobblemon.mod.common.client.render.models.blockbench.pokemon.PokemonPoseableModel;
import com.cobblemon.mod.common.client.render.models.blockbench.repository.PokemonModelRepository;
import com.cobblemon.mod.common.entity.pokemon.PokemonEntity;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonSpeciesManager;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.minecraft.resources.ResourceLocation;

public final class CobblemonVariantHelper {

  private static Set<ResourceLocation> femaleVariantSpecies;

  private CobblemonVariantHelper() {}

  public static boolean hasFemaleVariant(ResourceLocation speciesId) {
    if (femaleVariantSpecies == null) {
      femaleVariantSpecies = loadFemaleVariantSpecies();
    }
    return femaleVariantSpecies == null || femaleVariantSpecies.contains(speciesId);
  }

  private static Set<ResourceLocation> loadFemaleVariantSpecies() {
    Map<ResourceLocation, VaryingRenderableResolver<PokemonEntity, PokemonPoseableModel>>
        variations = PokemonModelRepository.INSTANCE.getVariations();
    if (variations.isEmpty()) {
      // Model repository is not loaded yet; keep all variants and retry on the next call.
      return null;
    }

    Set<ResourceLocation> speciesWithFemaleVariant = new HashSet<>();
    for (Map.Entry<ResourceLocation, VaryingRenderableResolver<PokemonEntity, PokemonPoseableModel>>
        entry : variations.entrySet()) {
      for (ModelAssetVariation variation : entry.getValue().getVariations()) {
        if (variation.getAspects().contains(CobblemonSpeciesManager.VARIANT_FEMALE)) {
          speciesWithFemaleVariant.add(entry.getKey());
          break;
        }
      }
    }
    return speciesWithFemaleVariant;
  }
}
