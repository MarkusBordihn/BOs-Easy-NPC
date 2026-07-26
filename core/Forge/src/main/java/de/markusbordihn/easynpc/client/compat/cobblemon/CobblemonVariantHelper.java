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
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import net.minecraft.resources.ResourceLocation;

public final class CobblemonVariantHelper {

  private static Map<ResourceLocation, Set<String>> speciesAspects;

  private CobblemonVariantHelper() {}

  public static boolean hasFemaleVariant(ResourceLocation speciesId) {
    return hasAspect(speciesId, CobblemonSpeciesManager.VARIANT_FEMALE);
  }

  public static boolean hasAspect(ResourceLocation speciesId, String aspect) {
    Map<ResourceLocation, Set<String>> aspectsBySpecies = getSpeciesAspects();
    if (aspectsBySpecies == null) {
      return true;
    }
    return aspectsBySpecies.getOrDefault(speciesId, Set.of()).contains(aspect);
  }

  public static Set<String> getAvailableAspects(ResourceLocation speciesId) {
    Map<ResourceLocation, Set<String>> aspectsBySpecies = getSpeciesAspects();
    if (aspectsBySpecies == null) {
      return Set.of();
    }
    return aspectsBySpecies.getOrDefault(speciesId, Set.of());
  }

  private static Map<ResourceLocation, Set<String>> getSpeciesAspects() {
    if (speciesAspects == null) {
      speciesAspects = loadSpeciesAspects();
    }
    return speciesAspects;
  }

  private static Map<ResourceLocation, Set<String>> loadSpeciesAspects() {
    Map<ResourceLocation, VaryingRenderableResolver<PokemonEntity, PokemonPoseableModel>>
        variations = PokemonModelRepository.INSTANCE.getVariations();
    if (variations.isEmpty()) {
      return null;
    }

    Map<ResourceLocation, Set<String>> aspectsBySpecies = new HashMap<>();
    for (Map.Entry<ResourceLocation, VaryingRenderableResolver<PokemonEntity, PokemonPoseableModel>>
        entry : variations.entrySet()) {
      Set<String> aspects = new LinkedHashSet<>();
      for (ModelAssetVariation variation : entry.getValue().getVariations()) {
        aspects.addAll(variation.getAspects());
      }
      if (!aspects.isEmpty()) {
        aspectsBySpecies.put(entry.getKey(), aspects);
      }
    }
    return aspectsBySpecies;
  }
}
