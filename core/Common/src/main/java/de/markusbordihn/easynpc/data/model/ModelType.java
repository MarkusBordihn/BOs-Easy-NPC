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

package de.markusbordihn.easynpc.data.model;

import java.util.EnumSet;
import java.util.Set;
import java.util.stream.Collectors;

public enum ModelType {
  ALLAY(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_WING,
          ModelPartType.LEFT_WING)),
  AVIAN(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_WING,
          ModelPartType.LEFT_WING,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG)),
  CANINE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG,
          ModelPartType.TAIL)),
  CREEPER(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG)),
  EQUINE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG,
          ModelPartType.TAIL)),
  FELINE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG,
          ModelPartType.TAIL1,
          ModelPartType.TAIL2)),
  GOLEM(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true),
  HUMANOID(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true),
  ILLAGER(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.ARMS,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true),
  PIXIE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_WING,
          ModelPartType.LEFT_WING)),
  QUADRUPED(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG)),
  SPIDER(EnumSet.of(ModelPartType.HEAD, ModelPartType.RIGHT_ARM, ModelPartType.LEFT_ARM)),
  VILLAGER(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.ARMS,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG)),
  ZOMBIE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true);

  private static final int PRIMARY_LIMIT = 6;

  private final Set<ModelPartType> modelParts;
  private final boolean requiresHatSync;

  ModelType(Set<ModelPartType> modelParts) {
    this(modelParts, false);
  }

  ModelType(Set<ModelPartType> modelParts, boolean requiresHatSync) {
    this.modelParts = modelParts;
    this.requiresHatSync = requiresHatSync;
  }

  public Set<ModelPartType> getModelParts() {
    return modelParts;
  }

  public Set<ModelPartType> getPrimaryModelParts() {
    if (modelParts.size() > PRIMARY_LIMIT) {
      return modelParts.stream()
          .limit(PRIMARY_LIMIT)
          .collect(Collectors.toCollection(() -> EnumSet.noneOf(ModelPartType.class)));
    }
    return modelParts;
  }

  public boolean requiresHatSync() {
    return this.requiresHatSync;
  }
}
