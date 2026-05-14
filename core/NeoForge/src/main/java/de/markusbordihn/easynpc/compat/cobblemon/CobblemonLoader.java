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
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.compat.IntegrationModelProvider;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CobblemonLoader implements IntegrationModelProvider {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final CobblemonLoader INSTANCE = new CobblemonLoader();

  private List<String> cachedModels;

  private CobblemonLoader() {}

  public static void register() {
    INSTANCE.cachedModels =
        PokemonSpecies.INSTANCE.getImplemented().stream()
            .map(species -> species.getResourceIdentifier().toString())
            .sorted()
            .collect(Collectors.toList());
    if (!INSTANCE.cachedModels.isEmpty()) {
      log.info("Loaded {} Cobblemon Species Models", INSTANCE.cachedModels.size());
    }
    IntegrationRegistry.register(INSTANCE);
  }

  @Override
  public String getIntegrationId() {
    return CobblemonSpeciesManager.INTEGRATION_ID;
  }

  @Override
  public List<String> getAvailableModels() {
    if (cachedModels == null || cachedModels.isEmpty()) {
      List<?> implemented = PokemonSpecies.INSTANCE.getImplemented();
      if (!implemented.isEmpty()) {
        log.info("Re-Loading Cobblemon Species Models ...");
        cachedModels =
            PokemonSpecies.INSTANCE.getImplemented().stream()
                .map(species -> species.getResourceIdentifier().toString())
                .sorted()
                .collect(Collectors.toList());
        log.info("Loaded {} Cobblemon Species Models", cachedModels.size());
      }
    }

    return cachedModels != null ? cachedModels : List.of();
  }
}
