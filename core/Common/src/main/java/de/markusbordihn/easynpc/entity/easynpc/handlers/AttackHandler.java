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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.NPCBaseConfig;
import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.data.attribute.CombatAttributes;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCBullet;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCItemProjectile;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCProjectile;
import de.markusbordihn.easynpc.handler.FactionHandler;
import de.markusbordihn.easynpc.item.ModItemTags;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import net.minecraft.core.Holder.Reference;
import net.minecraft.core.Holder;
import net.minecraft.core.component.BlockTransformer;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.tags.DamageTypeTags;
import net.minecraft.tags.ItemTags;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.entity.projectile.arrow.AbstractArrow;
import net.minecraft.world.item.BowItem;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ItemStackTemplate;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.item.component.BlockTransformers;
import net.minecraft.world.item.component.ChargedProjectiles;

public class AttackHandler {

  private AttackHandler() {}

  public static void addChargedProjectile(
      ItemStack weaponItemStack, ItemStack projectileItemStack) {
    weaponItemStack.set(
        DataComponents.CHARGED_PROJECTILES,
        ChargedProjectiles.of(ItemStackTemplate.fromNonEmptyStack(projectileItemStack)));
  }

  public static boolean isMeleeWeapon(ItemStack itemStack) {
    // Check vanilla weapon tags
    if (itemStack.is(ItemTags.SWORDS) || itemStack.is(ItemTags.AXES)) {
      return true;
    }

    // Check custom melee weapon tags
    if (itemStack.is(ModItemTags.MELEE_WEAPON)) {
      return true;
    }

    // Minecraft 26.3 removed AxeItem, so untagged axes are detected by their block transformer.
    Holder<BlockTransformer> blockTransformer = itemStack.get(DataComponents.BLOCK_TRANSFORMER);
    if (blockTransformer != null && blockTransformer.is(BlockTransformers.AXE)) {
      return true;
    }

    // Check item name for common melee weapon keywords
    Item item = itemStack.getItem();
    String itemName = item.toString().toLowerCase();
    return itemName.contains("sword")
        || itemName.contains("axe")
        || itemName.contains("blade")
        || itemName.contains("dagger")
        || itemName.contains("knife")
        || itemName.contains("spear")
        || itemName.contains("katana")
        || itemName.contains("rapier")
        || itemName.contains("saber");
  }

  public static boolean isBowWeapon(ItemStack itemStack) {
    return itemStack.getItem() instanceof BowItem || itemStack.is(ModItemTags.RANGED_WEAPON_BOW);
  }

  public static boolean isCrossbowWeapon(ItemStack itemStack) {
    return itemStack.getItem() instanceof CrossbowItem
        || itemStack.is(ModItemTags.RANGED_WEAPON_CROSSBOW);
  }

  public static boolean isGunWeapon(ItemStack itemStack) {
    return itemStack.is(ModItemTags.RANGED_WEAPON_GUN);
  }

  public static boolean canUseNonMeleeWeapon(ItemStack nonMeleeWeapon) {
    return nonMeleeWeapon.getItem() instanceof CrossbowItem
        || nonMeleeWeapon.getItem() instanceof BowItem;
  }

  public static boolean isHoldingBowWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isBowWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingCrossbowWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isCrossbowWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingGunWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isGunWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingMeleeWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isMeleeWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingProjectileWeapon(LivingEntity livingEntity) {
    return livingEntity != null
        && livingEntity.getMainHandItem().getItem() instanceof ProjectileWeaponItem;
  }

  public static boolean isHoldingWeapon(LivingEntity livingEntity) {
    return isHoldingMeleeWeapon(livingEntity) || isHoldingProjectileWeapon(livingEntity);
  }

  public static void performDefaultRangedAttack(
      LivingEntity livingEntity, LivingEntity targedtedLivingEntity, float damage) {
    if (isHoldingBowWeapon(livingEntity)) {
      performBowAttack(livingEntity, targedtedLivingEntity, damage);
    } else if (isHoldingCrossbowWeapon(livingEntity)) {
      performCrossbowAttack(livingEntity, targedtedLivingEntity);
    } else if (isHoldingGunWeapon(livingEntity)) {
      performGunAttack(livingEntity, targedtedLivingEntity, damage);
    }
  }

  public static InteractionHand getBowHoldingHand(LivingEntity livingEntity) {
    ItemStack itemStack = livingEntity.getMainHandItem();
    return isBowWeapon(itemStack) ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND;
  }

  public static InteractionHand getCrossbowHoldingHand(LivingEntity livingEntity) {
    ItemStack itemStack = livingEntity.getMainHandItem();
    return isCrossbowWeapon(itemStack) ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND;
  }

  public static InteractionHand getGunHoldingHand(LivingEntity livingEntity) {
    ItemStack itemStack = livingEntity.getMainHandItem();
    return isGunWeapon(itemStack) ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND;
  }

  public static void performGunAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget, float damage) {
    if (!isGunWeapon(livingEntity.getMainHandItem())) {
      return;
    }

    ProjectileData projectileData = getProjectileData(livingEntity);
    if (projectileData.enabled()) {
      performCustomProjectileAttack(livingEntity, livingEntityTarget, projectileData);
      return;
    }

    ItemStack itemStackWeapon = livingEntity.getItemInHand(getGunHoldingHand(livingEntity));
    AbstractArrow abstractArrow = getBullet(livingEntity, itemStackWeapon, damage);
    double targetX = livingEntityTarget.getX() - livingEntity.getX();
    double targetY = livingEntityTarget.getY() - abstractArrow.getY();
    double targetZ = livingEntityTarget.getZ() - livingEntity.getZ();
    double targetRadius = Math.sqrt(targetX * targetX + targetZ * targetZ);
    abstractArrow.shoot(
        targetX,
        targetY + targetRadius * 0.2F,
        targetZ,
        1.6F,
        14.0F - livingEntity.level().getDifficulty().getId() * 4);
    playRangedAttackSound(livingEntity, SoundEvents.FIRECHARGE_USE);
    livingEntity.level().addFreshEntity(abstractArrow);
  }

  public static void performCrossbowAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget) {
    ItemStack itemStack = livingEntity.getItemInHand(getCrossbowHoldingHand(livingEntity));
    ProjectileData projectileData = getProjectileData(livingEntity);
    if (projectileData.enabled()) {
      performCustomProjectileAttack(
          livingEntity, livingEntityTarget, projectileData, SoundEvents.CROSSBOW_SHOOT);
      if (itemStack.getItem() instanceof CrossbowItem) {
        itemStack.set(DataComponents.CHARGED_PROJECTILES, ChargedProjectiles.EMPTY);
      }
      return;
    }

    if (livingEntity instanceof CrossbowAttackMob crossbowAttackMob) {
      addChargedProjectile(itemStack, new ItemStack(Items.ARROW, 1));
      crossbowAttackMob.performCrossbowAttack(livingEntity, 1.6F);
    }
  }

  public static List<Projectile> performProjectileAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget) {
    return performCustomProjectileAttack(
        livingEntity,
        livingEntityTarget,
        getProjectileData(livingEntity),
        SoundEvents.FIRECHARGE_USE);
  }

  public static List<Projectile> performCustomProjectileAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget, ProjectileData projectileData) {
    return performCustomProjectileAttack(
        livingEntity, livingEntityTarget, projectileData, SoundEvents.FIRECHARGE_USE);
  }

  private static List<Projectile> performCustomProjectileAttack(
      LivingEntity livingEntity,
      LivingEntity livingEntityTarget,
      ProjectileData projectileData,
      SoundEvent fallbackSound) {
    List<Projectile> projectiles = new ArrayList<>(projectileData.shotCount());
    for (int i = 0; i < projectileData.shotCount(); i++) {
      Projectile projectile = createCustomProjectile(livingEntity, projectileData);
      if (!(projectile instanceof EasyNPCProjectile easyNPCProjectile)) {
        break;
      }

      double targetX = livingEntityTarget.getX() - projectile.getX();
      double targetY = livingEntityTarget.getY(0.5D) - projectile.getY();
      double targetZ = livingEntityTarget.getZ() - projectile.getZ();
      float velocity = projectileData.velocity();
      float gravityPerTick = easyNPCProjectile.getGravityPerTick();
      double gravityDrop =
          (targetX * targetX + targetZ * targetZ) * gravityPerTick / (2.0D * velocity * velocity);
      projectile.shoot(
          targetX, targetY + gravityDrop, targetZ, velocity, projectileData.inaccuracy());
      livingEntity.level().addFreshEntity(projectile);
      projectiles.add(projectile);
    }
    if (!projectiles.isEmpty()) {
      playRangedAttackSound(livingEntity, fallbackSound);
    }
    return projectiles;
  }

  public static Projectile createCustomProjectile(
      LivingEntity livingEntity, ProjectileData projectileData) {
    if (EasyNPCBullet.supportsProjectileType(projectileData.projectileType())) {
      return EasyNPCBullet.create(livingEntity, projectileData);
    }

    return EasyNPCItemProjectile.create(livingEntity, projectileData);
  }

  private static ProjectileData getProjectileData(LivingEntity livingEntity) {
    if (livingEntity instanceof EasyNPC<?> easyNPC) {
      AttackDataCapable<?> attackData = easyNPC.getEasyNPCAttackData();
      if (attackData != null) {
        return attackData.getProjectileData();
      }
    }
    return ProjectileData.DEFAULT;
  }

  private static void playRangedAttackSound(LivingEntity livingEntity, SoundEvent fallbackSound) {
    if (livingEntity instanceof EasyNPC<?> easyNPC) {
      SoundDataCapable<?> soundData = easyNPC.getEasyNPCSoundData();
      if (soundData != null && soundData.hasDefaultSound(SoundType.RANGED_ATTACK)) {
        soundData.playDefaultSound(SoundType.RANGED_ATTACK);
        return;
      }
    }
    livingEntity.playSound(
        fallbackSound, 1.0F, 1.0F / (livingEntity.getRandom().nextFloat() * 0.4F + 0.8F));
  }

  public static void performBowAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget, float damage) {
    if (!isBowWeapon(livingEntity.getMainHandItem())) {
      return;
    }

    ProjectileData projectileData = getProjectileData(livingEntity);
    if (projectileData.enabled()) {
      performCustomProjectileAttack(
          livingEntity, livingEntityTarget, projectileData, SoundEvents.SKELETON_SHOOT);
      return;
    }

    ItemStack itemStackWeapon = livingEntity.getItemInHand(getBowHoldingHand(livingEntity));
    ItemStack itemStackProjectile = livingEntity.getProjectile(itemStackWeapon);
    AbstractArrow abstractArrow =
        getArrow(
            livingEntity,
            itemStackWeapon,
            itemStackProjectile.isEmpty() ? new ItemStack(Items.ARROW) : itemStackProjectile,
            damage);
    double targetX = livingEntityTarget.getX() - livingEntity.getX();
    double targetY = livingEntityTarget.getY(0.3333333333333333D) - abstractArrow.getY();
    double targetZ = livingEntityTarget.getZ() - livingEntity.getZ();
    double targetRadius = Math.sqrt(targetX * targetX + targetZ * targetZ);
    abstractArrow.shoot(
        targetX,
        targetY + targetRadius * 0.2F,
        targetZ,
        1.6F,
        14.0F - livingEntity.level().getDifficulty().getId() * 4);
    livingEntity.playSound(
        SoundEvents.SKELETON_SHOOT,
        1.0F,
        1.0F / (livingEntity.getRandom().nextFloat() * 0.4F + 0.8F));
    livingEntity.level().addFreshEntity(abstractArrow);
  }

  public static AbstractArrow getArrow(
      LivingEntity livingEntity,
      ItemStack itemStackWeapon,
      ItemStack itemStackProjectile,
      float damage) {
    return ProjectileUtil.getMobArrow(
        livingEntity,
        itemStackProjectile.isEmpty() ? new ItemStack(Items.ARROW) : itemStackProjectile,
        damage,
        itemStackWeapon);
  }

  public static AbstractArrow getBullet(
      LivingEntity livingEntity, ItemStack itemStackWeapon, float damage) {
    Optional<Reference<Item>> item =
        BuiltInRegistries.ITEM.get(Identifier.fromNamespaceAndPath(Constants.MOD_ID, "bullet"));
    return item.map(
            itemReference ->
                ProjectileUtil.getMobArrow(
                    livingEntity,
                    itemReference != null && itemReference.value() != Items.AIR
                        ? new ItemStack(itemReference)
                        : new ItemStack(Items.ARROW),
                    damage,
                    itemStackWeapon))
        .orElseGet(
            () ->
                ProjectileUtil.getMobArrow(
                    livingEntity, new ItemStack(Items.ARROW), damage, itemStackWeapon));
  }

  public static boolean handleCanAttack(
      EasyNPCBase<?> easyNPC, LivingEntity livingEntity, boolean defaultValue) {
    return defaultValue
        || FactionHandler.canBypassInvulnerability(easyNPC.getLivingEntity(), livingEntity);
  }

  public static boolean handleIsInvulnerableTo(
      EasyNPCBase<?> easyNPC, DamageSource damageSource, boolean defaultValue) {
    // Allow certain damage types to bypass invulnerability like void or /kill command.
    if (NPCBaseConfig.ALLOW_BYPASS_INVULNERABILITY
        && damageSource.is(DamageTypeTags.BYPASSES_INVULNERABILITY)) {
      return defaultValue;
    }

    // Attackers from hostile factions (NPCs or players/entities in a hostile faction team)
    // bypass the invulnerability protection if the NPC allows it.
    if (damageSource.getEntity() instanceof LivingEntity attacker
        && FactionHandler.canBypassInvulnerability(attacker, easyNPC.getLivingEntity())) {
      return false;
    }

    // Players and monsters are controlled by their own attackable switches, independent of the
    // invulnerability protection.
    CombatAttributes combatAttributes = easyNPC.getEntityAttributes().getCombatAttributes();
    if (damageSource.getEntity() instanceof Player) {
      return !combatAttributes.isAttackableByPlayers();
    } else if (damageSource.getEntity() instanceof Monster) {
      return !combatAttributes.isAttackableByMonsters();
    }

    // Everything else, like environmental damage, is covered by the invulnerability protection.
    if (combatAttributes.isInvulnerable()) {
      return true;
    }

    return defaultValue;
  }
}
