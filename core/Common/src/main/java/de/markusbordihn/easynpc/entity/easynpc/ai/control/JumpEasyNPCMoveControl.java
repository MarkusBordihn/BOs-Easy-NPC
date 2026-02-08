package de.markusbordihn.easynpc.entity.easynpc.ai.control;

import de.markusbordihn.easynpc.api.npc.base.slime.SlimeBase;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.control.MoveControl;

public class JumpEasyNPCMoveControl extends MoveControl {

  private static final int DEFAULT_JUMP_DELAY = 10;
  private static final float AGGRESSIVE_JUMP_DELAY_FACTOR = 3.0F;

  private float yRot;
  private int jumpDelay;
  private boolean isAggressive;

  public JumpEasyNPCMoveControl(Mob mob) {
    super(mob);
    this.yRot = 180.0F * mob.getYRot() / (float) Math.PI;
  }

  public void setDirection(float yRot, boolean isAggressive) {
    this.yRot = yRot;
    this.isAggressive = isAggressive;
  }

  public void setWantedMovement(double speedModifier) {
    this.speedModifier = speedModifier;
    this.operation = MoveControl.Operation.MOVE_TO;
  }

  @Override
  public void tick() {
    this.mob.setYRot(this.rotlerp(this.mob.getYRot(), this.yRot, 90.0F));
    this.mob.yHeadRot = this.mob.getYRot();
    this.mob.yBodyRot = this.mob.getYRot();

    if (this.operation != MoveControl.Operation.MOVE_TO) {
      this.mob.setZza(0.0F);
      return;
    }

    this.operation = MoveControl.Operation.WAIT;
    double moveSpeed = this.speedModifier * this.mob.getAttributeValue(Attributes.MOVEMENT_SPEED);
    this.mob.setSpeed((float) moveSpeed);

    if (!this.mob.onGround()) {
      return;
    }

    if (this.jumpDelay-- > 0) {
      this.mob.xxa = 0.0F;
      this.mob.zza = 0.0F;
      this.mob.setSpeed(0.0F);
      return;
    }

    if (this.mob instanceof SlimeBase slimeBase) {
      this.jumpDelay = slimeBase.getSlimeJumpDelay();
      if (this.isAggressive) {
        this.jumpDelay = (int) (this.jumpDelay / AGGRESSIVE_JUMP_DELAY_FACTOR);
      }
      this.mob.getJumpControl().jump();
      if (slimeBase.shouldPlayJumpSound()) {
        this.mob.playSound(SoundEvents.SLIME_JUMP, 1.0F, 1.0F);
      }
    } else {
      this.jumpDelay =
          this.isAggressive
              ? (int) (DEFAULT_JUMP_DELAY / AGGRESSIVE_JUMP_DELAY_FACTOR)
              : DEFAULT_JUMP_DELAY;
      this.mob.getJumpControl().jump();
    }
  }
}
