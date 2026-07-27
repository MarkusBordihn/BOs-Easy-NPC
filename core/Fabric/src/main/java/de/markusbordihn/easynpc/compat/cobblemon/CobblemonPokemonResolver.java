/*
 * Copyright 2023 Markus Bordihn
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
import com.cobblemon.mod.common.pokemon.Gender;
import com.cobblemon.mod.common.pokemon.Pokemon;
import com.cobblemon.mod.common.pokemon.Species;
import de.markusbordihn.easynpc.Constants;
import java.util.Set;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class CobblemonPokemonResolver {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private CobblemonPokemonResolver() {}

  public static Pokemon resolvePokemon(ResourceLocation modelKey) {
    ResourceLocation speciesId = CobblemonSpeciesManager.getBaseSpeciesId(modelKey);
    Species species = PokemonSpecies.INSTANCE.getByIdentifier(speciesId);
    if (species == null) {
      log.warn("Unknown Cobblemon species: {}", speciesId);
      return null;
    }
    Pokemon pokemon = new Pokemon();
    pokemon.setSpecies(species);
    applyVariantAspects(pokemon, modelKey);
    return pokemon;
  }

  public static void applyVariantAspects(Pokemon pokemon, ResourceLocation modelKey) {
    Set<String> variantAspects = CobblemonSpeciesManager.getVariantAspects(modelKey);
    float maleRatio = pokemon.getSpecies().getMaleRatio();
    if (CobblemonSpeciesManager.isGenderMutable(maleRatio)) {
      pokemon.setGender(
          CobblemonSpeciesManager.shouldApplyFemaleGender(variantAspects)
              ? Gender.FEMALE
              : Gender.MALE);
    }
    if (CobblemonSpeciesManager.shouldApplyShiny(variantAspects)) {
      pokemon.setShiny(true);
    }
    pokemon.updateAspects();
  }
}
