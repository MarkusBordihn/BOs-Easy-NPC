/**
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

package de.markusbordihn.easynpc.entity.npc;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.control.FlyingMoveControl;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.skin.SkinModel;

public class Fairy extends EasyNPCEntity {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Constants values
  private static final float DEFAULT_SCALE_X = 0.4f;
  private static final float DEFAULT_SCALE_Y = 0.4f;
  private static final float DEFAULT_SCALE_Z = 0.4f;

  // General Information
  public static final String ID = "fairy";
  public static final String NAME = "Fairy";

  // Skin Details
  public enum Variant {
    GREEN, RED, BLUE
  }

  public Fairy(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
    this.moveControl = new FlyingMoveControl(this, 20, true);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes().add(Attributes.MOVEMENT_SPEED, 0.5F)
        .add(Attributes.MAX_HEALTH, 16.0D).add(Attributes.ATTACK_DAMAGE, 0.0D);
  }

  @Override
  public Float getDefaultScaleX() {
    return Fairy.DEFAULT_SCALE_X;
  }

  @Override
  public Float getDefaultScaleY() {
    return Fairy.DEFAULT_SCALE_Y;
  }

  @Override
  public Float getDefaultScaleZ() {
    return Fairy.DEFAULT_SCALE_Z;
  }

  @Override
  public SkinModel getSkinModel() {
    return SkinModel.FAIRY;
  }

  @Override
  public boolean hasLeftLegModelPart() {
    return false;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.GREEN;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public int getEntityGuiScaling() {
    return 70;
  }

  @Override
  public int getEntityGuiTop() {
    return 0;
  }

  @Override
  public int getEntityDialogTop() {
    return -37;
  }

  @Override
  public int getEntityDialogScaling() {
    return 75;
  }

}
