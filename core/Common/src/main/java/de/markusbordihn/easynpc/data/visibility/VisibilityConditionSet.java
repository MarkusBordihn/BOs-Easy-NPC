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

package de.markusbordihn.easynpc.data.visibility;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;

public record VisibilityConditionSet(List<ConditionDataEntry> conditions) {

  public static final int MAXIMUM_CONDITIONS = 16;
  public static final VisibilityConditionSet EMPTY = new VisibilityConditionSet(List.of());
  public static final Set<ConditionType> UNSUPPORTED_CONDITION_TYPES =
      EnumSet.of(ConditionType.CHANCE, ConditionType.EXECUTION_LIMIT, ConditionType.FALLBACK);

  private static final String CONDITIONS_TAG = "Conditions";

  public static final Codec<VisibilityConditionSet> CODEC =
      RecordCodecBuilder.create(
          instance ->
              instance
                  .group(
                      CompoundTag.CODEC
                          .xmap(ConditionDataEntry::new, ConditionDataEntry::createTag)
                          .listOf()
                          .optionalFieldOf(CONDITIONS_TAG, List.of())
                          .forGetter(VisibilityConditionSet::conditions))
                  .apply(instance, VisibilityConditionSet::new));

  public VisibilityConditionSet {
    conditions =
        conditions.stream()
            .filter(VisibilityConditionSet::isSupported)
            .distinct()
            .limit(MAXIMUM_CONDITIONS)
            .toList();
  }

  public static boolean isSupported(ConditionDataEntry conditionDataEntry) {
    return conditionDataEntry != null
        && conditionDataEntry.isValid()
        && !UNSUPPORTED_CONDITION_TYPES.contains(conditionDataEntry.conditionType());
  }

  public static VisibilityConditionSet fromConditionDataSet(ConditionDataSet conditionDataSet) {
    return conditionDataSet == null
        ? EMPTY
        : new VisibilityConditionSet(conditionDataSet.getConditionsList());
  }

  public static VisibilityConditionSet fromTag(CompoundTag compoundTag) {
    return CODEC.parse(NbtOps.INSTANCE, compoundTag).result().orElse(EMPTY);
  }

  public boolean isEmpty() {
    return this.conditions.isEmpty();
  }

  public Set<ConditionDataEntry> conditionSet() {
    return new LinkedHashSet<>(this.conditions);
  }

  public ConditionDataSet toConditionDataSet() {
    return new ConditionDataSet(this.conditionSet());
  }

  public VisibilityConditionSet withConditions(List<ConditionDataEntry> conditions) {
    return new VisibilityConditionSet(conditions);
  }

  public CompoundTag createTag() {
    return CODEC
        .encodeStart(NbtOps.INSTANCE, this)
        .result()
        .filter(CompoundTag.class::isInstance)
        .map(CompoundTag.class::cast)
        .orElseGet(CompoundTag::new);
  }
}
