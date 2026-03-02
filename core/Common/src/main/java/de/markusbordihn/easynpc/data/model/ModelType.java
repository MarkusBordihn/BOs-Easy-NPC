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
          ModelPartType.LEFT_WING),
      false,
      null,
      null),
  AVIAN(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_WING,
          ModelPartType.LEFT_WING,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      false,
      new ItemAttachmentPoint(
          ModelPartType.RIGHT_WING, 0.0F, 5.0F, -1.0F, (float) (-Math.PI / 3), 0.0F, 0.0F, 0.5F),
      null),
  CANINE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG,
          ModelPartType.TAIL),
      false,
      new ItemAttachmentPoint(
          ModelPartType.HEAD, 0.0F, 0.0F, -1.0F, 0.0F, 0.0F, (float) (Math.PI / 2), 0.5F),
      null),
  CREEPER(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG),
      false,
      new ItemAttachmentPoint(
          ModelPartType.BODY, 3.0F, 6.0F, -3.0F, (float) (-Math.PI / 3), 0.0F, 0.0F, 0.5F),
      null),
  EQUINE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG,
          ModelPartType.TAIL),
      false,
      new ItemAttachmentPoint(
          ModelPartType.HEAD, 0.0F, 4.0F, -14.0F, 0.0F, 0.0F, (float) (Math.PI / 2), 0.5F),
      null),
  FELINE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG,
          ModelPartType.TAIL1,
          ModelPartType.TAIL2),
      false,
      new ItemAttachmentPoint(
          ModelPartType.HEAD, 0.0F, 0.0F, -3.0F, 0.0F, 0.0F, (float) (Math.PI / 2), 0.45F),
      null),
  GOLEM(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true,
      new ItemAttachmentPoint(
          ModelPartType.RIGHT_ARM, -11.0F, 25.0F, -3.0F, (float) (-Math.PI / 2), 0.0F, 0.0F, 0.75F),
      null),
  ENDERMAN(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true,
      new ItemAttachmentPoint(
          ModelPartType.RIGHT_ARM, 0.0F, 26.0F, 0.0F, (float) (-Math.PI / 2), 0.0F, 0.0F, 0.6F),
      null),
  HUMANOID(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true,
      null,
      null),
  ILLAGER(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.ARMS,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true,
      new ItemAttachmentPoint(
          ModelPartType.BODY, 0.0F, 8.0F, -5.0F, (float) (-Math.PI / 2), 0.0F, 0.0F, 0.6F),
      null),
  PIXIE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_WING,
          ModelPartType.LEFT_WING),
      false,
      null,
      null),
  QUADRUPED(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_FRONT_LEG,
          ModelPartType.LEFT_FRONT_LEG,
          ModelPartType.RIGHT_HIND_LEG,
          ModelPartType.LEFT_HIND_LEG),
      false,
      new ItemAttachmentPoint(
          ModelPartType.HEAD, 0.0F, 3.0F, -10.0F, 0.0F, 0.0F, (float) (Math.PI / 2), 0.5F),
      null),
  SPIDER(
      EnumSet.of(ModelPartType.HEAD, ModelPartType.RIGHT_ARM, ModelPartType.LEFT_ARM),
      false,
      new ItemAttachmentPoint(
          ModelPartType.RIGHT_ARM, 0.0F, 10.0F, -3.0F, (float) (-Math.PI / 2), 0.0F, 0.0F, 0.5F),
      null),
  SLIME(
      EnumSet.of(ModelPartType.BODY),
      false,
      new ItemAttachmentPoint(
          ModelPartType.BODY, 5.0F, 18.0F, -3.0F, (float) (-Math.PI / 3), 0.0F, 0.0F, 0.5F),
      null),
  GHAST(
      EnumSet.of(ModelPartType.BODY, ModelPartType.LEFT_ARM, ModelPartType.RIGHT_ARM),
      false,
      new ItemAttachmentPoint(
          ModelPartType.RIGHT_ARM, 0.0F, 8.0F, 0.0F, (float) (-Math.PI / 2), 0.0F, 0.0F, 0.6F),
      null),
  VILLAGER(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.ARMS,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      false,
      null,
      null),
  ZOMBIE(
      EnumSet.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG),
      true,
      null,
      null);

  private static final int PRIMARY_LIMIT = 6;

  private final Set<ModelPartType> modelParts;
  private final boolean requiresHatSync;
  private final ItemAttachmentPoint mainHandAttachment;
  private final ItemAttachmentPoint offHandAttachment;

  ModelType(Set<ModelPartType> modelParts) {
    this(modelParts, false, null, null);
  }

  ModelType(Set<ModelPartType> modelParts, boolean requiresHatSync) {
    this(modelParts, requiresHatSync, null, null);
  }

  ModelType(
      Set<ModelPartType> modelParts,
      boolean requiresHatSync,
      ItemAttachmentPoint mainHandAttachment,
      ItemAttachmentPoint offHandAttachment) {
    this.modelParts = modelParts;
    this.requiresHatSync = requiresHatSync;
    this.mainHandAttachment = mainHandAttachment;
    this.offHandAttachment = offHandAttachment;
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

  public ItemAttachmentPoint getMainHandAttachment() {
    return mainHandAttachment;
  }

  public ItemAttachmentPoint getOffHandAttachment() {
    return offHandAttachment;
  }

  /**
   * Returns true if this model type has custom item attachment points defined. Model types that
   * return false (HUMANOID, ZOMBIE, VILLAGER) either use vanilla ItemInHandLayer or don't support
   * held items.
   */
  public boolean hasItemAttachment() {
    return mainHandAttachment != null || offHandAttachment != null;
  }
}
