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

package de.markusbordihn.easynpc.api.preset;

import de.markusbordihn.easynpc.api.action.ActionRegistry;
import de.markusbordihn.easynpc.api.condition.ConditionRegistry;
import java.util.Set;
import java.util.function.Predicate;
import net.minecraft.resources.ResourceLocation;

/** Offline validation accepts valid registry identifiers when no server registry is available. */
public record PresetValidationContext(
    Predicate<String> knownEntityTypes,
    Predicate<ResourceLocation> knownCustomActions,
    Predicate<ResourceLocation> knownCustomConditions,
    boolean expectsIdentityFreePreset) {

  public PresetValidationContext {
    if (knownEntityTypes == null) {
      knownEntityTypes = entityTypeId -> true;
    }
    if (knownCustomActions == null) {
      knownCustomActions = actionId -> true;
    }
    if (knownCustomConditions == null) {
      knownCustomConditions = conditionId -> true;
    }
  }

  public static PresetValidationContext offline() {
    return new PresetValidationContext(null, null, null, false);
  }

  public static PresetValidationContext forKnownEntityTypes(Set<String> entityTypeIds) {
    return new PresetValidationContext(entityTypeIds::contains, null, null, false);
  }

  public static PresetValidationContext forServer(Set<String> entityTypeIds) {
    return new PresetValidationContext(
        entityTypeIds::contains,
        ActionRegistry::isRegistered,
        ConditionRegistry::isRegistered,
        false);
  }

  public PresetValidationContext withIdentityFreePreset() {
    return new PresetValidationContext(
        this.knownEntityTypes, this.knownCustomActions, this.knownCustomConditions, true);
  }
}
