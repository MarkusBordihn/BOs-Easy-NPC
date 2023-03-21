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

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.skin.SkinModel;
import de.markusbordihn.easynpc.utils.TextUtils;

public class Villager extends EasyNPCEntity {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // General Information
  public static final String ID = "villager";
  public static final String NAME = "Villager";

  // Variants
  public enum Variant {
    DEFAULT, DESERT, JUNGLE, PLAINS, SAVANNA, SNOW, SWAMP, TAIGA
  }

  public Villager(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes().add(Attributes.MOVEMENT_SPEED, 0.5F)
        .add(Attributes.MAX_HEALTH, 16.0D).add(Attributes.ATTACK_DAMAGE, 0.0D);
  }

  @Override
  public SkinModel getSkinModel() {
    return SkinModel.VILLAGER;
  }

  @Override
  public boolean hasBodyModelPart() {
    return false;
  }

  @Override
  public boolean hasLeftArmModelPart() {
    return false;
  }

  @Override
  public boolean hasRightArmModelPart() {
    return false;
  }

  @Override
  public boolean canUseArmor() {
    return false;
  }

  @Override
  public boolean canUseOffHand() {
    return false;
  }

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    if (component != null) {
      return TextUtils.removeAction(component);
    }
    Component professionName = getProfessionName();
    Component variantName = getVariantName();
    return new TextComponent(variantName.getString() + " (" + professionName.getString() + ")");
  }

  @Override
  public boolean hasProfessions() {
    return true;
  }

  @Override
  public boolean hasProfession() {
    return true;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.DEFAULT;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

}
