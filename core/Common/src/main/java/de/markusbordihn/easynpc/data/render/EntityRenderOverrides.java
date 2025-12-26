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

package de.markusbordihn.easynpc.data.render;

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinType;
import java.util.UUID;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Pose;

public record EntityRenderOverrides(
    CustomRotation rootRotation,
    CustomScale rootScale,
    ModelPose modelPose,
    Pose entityPose,
    Boolean invisible,
    Boolean hideNameTag,
    RenderType renderType,
    EntityType<?> renderEntityType,
    SkinType skinType,
    UUID skinUUID,
    Enum<?> variant,
    Profession profession) {

  public static final EntityRenderOverrides NONE =
      new EntityRenderOverrides(
          null, null, null, null, null, null, null, null, null, null, null, null);

  public static final EntityRenderOverrides HIDE_NAME_TAG = NONE.withHideNameTag(true);

  public static final EntityRenderOverrides RESET_ROOT =
      NONE.withRootRotation(new CustomRotation(0.0F, 0.0F, 0.0F))
          .withRootScale(new CustomScale(1.0F, 1.0F, 1.0F));

  public static final EntityRenderOverrides HIDE_NAME_TAG_RESET_ROOT =
      RESET_ROOT.withHideNameTag(true);

  public static EntityRenderOverrides withSkin(SkinType skinType, UUID skinUUID) {
    return NONE.withHideNameTag(true)
        .withSkinType(skinType)
        .withSkinUUID(skinUUID)
        .withRootScale(new CustomScale(1.0F, 1.0F, 1.0F));
  }

  public static EntityRenderOverrides withVariant(Enum<?> variant, Profession profession) {
    return NONE.withHideNameTag(true)
        .withSkinType(SkinType.DEFAULT)
        .withVariant(variant)
        .withProfession(profession)
        .withRootScale(new CustomScale(1.0F, 1.0F, 1.0F));
  }

  public static EntityRenderOverrides withCustomModel(
      RenderType renderType, EntityType<?> entityType) {
    return NONE.withRenderType(renderType, entityType);
  }

  public EntityRenderOverrides withRootRotation(CustomRotation rotation) {
    return new EntityRenderOverrides(
        rotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withRootScale(CustomScale scale) {
    return new EntityRenderOverrides(
        this.rootRotation,
        scale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withModelPose(ModelPose modelPose) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withEntityPose(Pose entityPose) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withInvisible(boolean invisible) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withHideNameTag(boolean hide) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        hide,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withRenderType(RenderType renderType, EntityType<?> entityType) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        renderType,
        entityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withSkinType(SkinType skinType) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        skinType,
        this.skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withSkinUUID(UUID skinUUID) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        skinUUID,
        this.variant,
        this.profession);
  }

  public EntityRenderOverrides withVariant(Enum<?> variant) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        variant,
        this.profession);
  }

  public EntityRenderOverrides withProfession(Profession profession) {
    return new EntityRenderOverrides(
        this.rootRotation,
        this.rootScale,
        this.modelPose,
        this.entityPose,
        this.invisible,
        this.hideNameTag,
        this.renderType,
        this.renderEntityType,
        this.skinType,
        this.skinUUID,
        this.variant,
        profession);
  }
}
