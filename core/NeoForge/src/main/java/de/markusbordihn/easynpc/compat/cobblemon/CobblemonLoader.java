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

package de.markusbordihn.easynpc.compat.cobblemon;

import com.cobblemon.mod.common.api.pokemon.PokemonSpecies;
import com.cobblemon.mod.common.pokemon.Species;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.compat.IntegrationModelProvider;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CobblemonLoader implements IntegrationModelProvider {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final CobblemonLoader INSTANCE = new CobblemonLoader();

  // The default filter keeps all female variants so the server-side list stays a superset for
  // model validation; the client injects a resolver-based filter for the actual selection list.
  private static Predicate<ResourceLocation> femaleVariantFilter = speciesId -> true;

  private List<ResourceLocation> cachedModels;

  private CobblemonLoader() {}

  public static void setFemaleVariantFilter(Predicate<ResourceLocation> filter) {
    if (filter != null) {
      femaleVariantFilter = filter;
    }
  }

  public static void register() {
    INSTANCE.cachedModels = loadSpeciesModels();
    if (!INSTANCE.cachedModels.isEmpty()) {
      log.info("Loaded {} Cobblemon Species Models", INSTANCE.cachedModels.size());
    }
    IntegrationRegistry.register(INSTANCE);
  }

  private static List<ResourceLocation> loadSpeciesModels() {
    List<ResourceLocation> speciesModels = new ArrayList<>();
    for (Species species : PokemonSpecies.INSTANCE.getImplemented()) {
      ResourceLocation speciesId = species.getResourceIdentifier();
      speciesModels.add(speciesId);
      speciesModels.add(
          CobblemonSpeciesManager.createVariantKey(
              speciesId, CobblemonSpeciesManager.VARIANT_SHINY));
      float maleRatio = species.getMaleRatio();
      if (maleRatio > 0.0F && maleRatio < 1.0F && femaleVariantFilter.test(speciesId)) {
        speciesModels.add(
            CobblemonSpeciesManager.createVariantKey(
                speciesId, CobblemonSpeciesManager.VARIANT_FEMALE));
        speciesModels.add(
            CobblemonSpeciesManager.createVariantKey(
                speciesId,
                CobblemonSpeciesManager.VARIANT_FEMALE,
                CobblemonSpeciesManager.VARIANT_SHINY));
      }
    }
    speciesModels.sort(Comparator.comparing(ResourceLocation::toString));
    return speciesModels;
  }

  @Override
  public String getIntegrationId() {
    return CobblemonSpeciesManager.INTEGRATION_ID;
  }

  @Override
  public List<String> getAvailableModels() {
    if (cachedModels == null || cachedModels.isEmpty()) {
      log.debug("Re-Loading Cobblemon Species Models ...");
      List<ResourceLocation> speciesModels = loadSpeciesModels();
      if (!speciesModels.isEmpty()) {
        this.cachedModels = speciesModels;
        log.debug("Loaded {} Cobblemon Species Models", cachedModels.size());
      }
    }

    if (cachedModels == null) {
      return List.of();
    }
    return cachedModels.stream().map(ResourceLocation::toString).toList();
  }
}
