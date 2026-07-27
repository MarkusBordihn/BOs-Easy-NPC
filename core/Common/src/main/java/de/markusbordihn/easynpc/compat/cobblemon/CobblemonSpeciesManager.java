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

import de.markusbordihn.easynpc.compat.CompatConstants;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import net.minecraft.resources.ResourceLocation;

public final class CobblemonSpeciesManager {

  public static final String INTEGRATION_ID = CompatConstants.MOD_COBBLEMON_ID;
  public static final String DEFAULT_MODEL = INTEGRATION_ID + ":ditto";
  public static final String VARIANT_FEMALE = "female";
  public static final String VARIANT_SHINY = "shiny";

  private static final List<String> VARIANT_TOKENS = List.of(VARIANT_FEMALE, VARIANT_SHINY);
  private static final char ASPECT_SEPARATOR = '.';
  private static final Pattern SUPPORTED_ASPECT = Pattern.compile("[a-z0-9_-]+");

  private static Set<ResourceLocation> knownSpecies = Set.of();

  private CobblemonSpeciesManager() {}

  public static boolean isSupportedAspect(String aspect) {
    return aspect != null && !aspect.isEmpty() && SUPPORTED_ASPECT.matcher(aspect).matches();
  }

  public static ResourceLocation getBaseSpeciesId(ResourceLocation modelKey) {
    String basePath = splitSpeciesPath(modelKey, null);
    if (basePath.equals(modelKey.getPath())) {
      return modelKey;
    }
    return ResourceLocation.fromNamespaceAndPath(modelKey.getNamespace(), basePath);
  }

  public static Set<String> getVariantAspects(ResourceLocation modelKey) {
    Set<String> variantAspects = new LinkedHashSet<>();
    splitSpeciesPath(modelKey, variantAspects);
    return variantAspects;
  }

  public static boolean isGenderMutable(float maleRatio) {
    return maleRatio > 0.0F && maleRatio < 1.0F;
  }

  public static boolean shouldApplyFemaleGender(Set<String> variantAspects) {
    return variantAspects.contains(VARIANT_FEMALE);
  }

  public static boolean shouldApplyShiny(Set<String> variantAspects) {
    return variantAspects.contains(VARIANT_SHINY);
  }

  public static ResourceLocation createVariantKey(
      ResourceLocation speciesId, String... variantTokens) {
    StringBuilder path = new StringBuilder(speciesId.getPath());
    for (String variantToken : variantTokens) {
      path.append(ASPECT_SEPARATOR).append(variantToken);
    }
    return ResourceLocation.fromNamespaceAndPath(speciesId.getNamespace(), path.toString());
  }

  public static void setKnownSpecies(Set<ResourceLocation> species) {
    knownSpecies = species != null ? Set.copyOf(species) : Set.of();
  }

  private static boolean isKnownSpecies(ResourceLocation speciesId) {
    return knownSpecies.contains(speciesId);
  }

  private static String splitSpeciesPath(ResourceLocation modelKey, Set<String> collectedAspects) {
    String path = modelKey.getPath();

    int separatorIndex = path.indexOf(ASPECT_SEPARATOR);
    if (separatorIndex > 0) {
      if (collectedAspects != null) {
        collectAspects(path.substring(separatorIndex + 1), collectedAspects);
      }
      return path.substring(0, separatorIndex);
    }

    if (isKnownSpecies(modelKey)) {
      return path;
    }

    return stripKnownVariantTokens(path, collectedAspects);
  }

  private static void collectAspects(String aspectPath, Set<String> collectedAspects) {
    for (String aspect : aspectPath.split("\\" + ASPECT_SEPARATOR)) {
      if (!aspect.isEmpty()) {
        collectedAspects.add(aspect);
      }
    }
  }

  private static String stripKnownVariantTokens(String path, Set<String> collectedAspects) {
    boolean stripped = true;
    while (stripped) {
      stripped = false;
      for (String variantToken : VARIANT_TOKENS) {
        String variantSuffix = '_' + variantToken;
        if (path.endsWith(variantSuffix)) {
          path = path.substring(0, path.length() - variantSuffix.length());
          if (collectedAspects != null) {
            collectedAspects.add(variantToken);
          }
          stripped = true;
        }
      }
    }
    return path;
  }
}
